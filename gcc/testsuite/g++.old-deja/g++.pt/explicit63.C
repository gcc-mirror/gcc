extern "C" void abort ();

template <class T> void f ()
{
}


template <class T> class C
{
  friend void f<T> ();
  public:
    void ff () { f<T> (); }
};

int main ()
{
  C<int> c;
  c.ff();
}
