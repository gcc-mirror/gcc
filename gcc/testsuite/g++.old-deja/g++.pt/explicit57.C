// { dg-do run  }
extern "C" void abort ();

int a = 0;

template <class T> void f ();
template <class T> void g ()
{
  if (a)
    abort ();
}

template <> void g<char> ()
{
}

template <class T> class C
{
  public:
    void ff () { f<T> (); }
    void gg () { g<T> (); }
};

template <class T> void f ()
{
  if (a)
    abort ();
}

template <> void f<char> ()
{
}

int main ()
{
  C<int> c;
  c.ff();
  c.gg();
  a = 1;
  C<char> d;
  d.ff();
  d.gg();
}
