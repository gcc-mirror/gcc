// { dg-do run  }
extern "C" void abort ();

template <class T> void f ()
{
  abort ();
}

template <> void f<char> ()
{
  abort ();
}

template <class T> void f (int)
{
  abort ();
}

template <> void f<char> (int)
{
}

template <class T> class C
{
  friend void f<char> (int);
  public:
    void ff () { f<char> (0); }
};

int main ()
{
  C<int> c;
  c.ff();
}
