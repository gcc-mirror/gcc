// { dg-do link }

class Foo {
public:
  // No out-of-class definition is provided for these class members.
  // That's technically a violation of the standard, but no diagnostic
  // is required, and, as a QOI issue, we should optimize away all
  // references.
  static const int erf = 0;
  static const int foo = 1;
};

int one()
{
  return Foo::foo;
}

int two()
{
  return Foo::foo + Foo::erf;
}

int three(int x)
{
  return x ? Foo::erf : Foo::foo;
}

int i;

int main ()
{
  one ();
  two ();
  three (i);
}
