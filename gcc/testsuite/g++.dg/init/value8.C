// PR c++/45315

struct A
{
  A ();
};

template < int > struct B : A
{
  void foo ()
  {
    new B < 0 > ();
  }
};

int main()
{
  B<1>().foo();
}
