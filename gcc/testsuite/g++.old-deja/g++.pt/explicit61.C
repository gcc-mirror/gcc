extern "C" void abort ();

template <class T> void f ();
template <class T> void g ()
{
  abort ();
}

template <> void g<char> ()
{
  abort ();
}

template <class T> class C
{
  public:
    void ff () { f<T> (0); }
    void gg () { g<T> (1); }
    template <class U> void f () { abort(); }
    template <class U> void g () { abort(); }
    template <class U> void f (int) {}
    template <class U> void g (int) {}
};

template <class T> void f ()
{
  abort ();
}

template <> void f<char> ()
{
  abort ();
}

int main ()
{
  C<int> c;
  c.ff();
  c.gg();
  C<char> d;
  d.ff();
  d.gg();
}
