extern "C" void abort ();

template <class T> void f ()
{
  abort ();
}

template <> void f<char> ()
{
}

template <class T> class C
{
  friend void f<char> ();
  public:
    void ff () { f<char> (); }
};

int main ()
{
  C<int> c;
  c.ff();
}
