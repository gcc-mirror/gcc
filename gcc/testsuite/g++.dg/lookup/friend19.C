// Make sure unhiding friends doesn't unhide similarly named friends

struct X
{
  friend int foo (X);
};

struct Y
{
  friend int foo (Y);
};

void Baz ()
{
  foo (X());
  foo (Y());
}

int foo (Y);
int foo (int);
// foo(X) still hidden

void Bar ()
{
  foo (X());
  foo (Y());
  ::foo (X()); // { dg-error "" }
  ::foo (Y());
}
