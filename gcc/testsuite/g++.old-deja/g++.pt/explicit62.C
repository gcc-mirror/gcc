// { dg-do run  }
extern "C" void abort ();

template <class T> void f ()
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
