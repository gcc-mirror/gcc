// { dg-lto-do link }

template < typename > struct X
{
  template < typename > static int test ();
  static const int i = sizeof (test < int >());
};

template struct X < int >;

int main()
{
  return 0;
}
