// PR c++/36364
// { dg-options "-frepo" }
// { dg-final { cleanup-repo-files } }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

template <typename C> struct A
{
  static void assign (C &c1, const C &c2) { c1 = c2; }
};

template <typename C, typename T> struct B
{
  struct D
  {
    static const C terminal;
    static unsigned long stor[];
    static D &empty_rep ()
    {
      void *p = reinterpret_cast <void *>(&stor);
      return *reinterpret_cast <D *>(p);
    }
    void test (unsigned long n)
    {
      T::assign (this->refdata ()[n], terminal);
    }
    C *refdata () throw ()
    {
      return reinterpret_cast <C *>(this + 1);
    }
  };
  C *dataplus;
  C *data () const { return dataplus; }
  D *rep () const { return &((reinterpret_cast < D * >(data ()))[-1]); }
  static D & empty_rep () { return D::empty_rep (); }
  B () : dataplus (empty_rep ().refdata ()) { }
  ~B () { }
  void push_back (C c) { rep ()->test (10); }
};

template <typename C, typename T> const C B <C, T>::D::terminal = C ();
template <typename C, typename T> unsigned long B <C, T>::D::stor[64];

int
main ()
{
  B <char, A <char> > s;
  s.push_back ('a');
}
