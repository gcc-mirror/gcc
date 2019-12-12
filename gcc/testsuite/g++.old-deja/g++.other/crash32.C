// { dg-do assemble }
// Origin: Jakub Jelinek <jakub@redhat.com>

struct foo
{
  enum e
  {
    not				// { dg-error "" }
    // We think the next close-brace closes the definition of struct
    // foo, not enum e.  Things go downhill from there
  }; // { dg-bogus "" } 
  ~foo(); // { dg-bogus "" "" { xfail *-*-* } } 
  void x (foo *&a, bool b = (unsigned char)0);
}; // { dg-bogus "" "" { xfail *-*-* } } 

namespace N
{
  struct bar;

  template<class T>
  struct baz
  {
    baz(T *p);
  };

  typedef baz<bar> c;
}

{ // { dg-error "expected" }
  int a;
};
