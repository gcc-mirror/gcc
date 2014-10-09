// { dg-do compile { target c++14 } }

class [[deprecated]] A
{
};

[[deprecated]]
int
foo(int n)
{
  return 42 + n;
}

class [[deprecated("B has been superceded by C")]] B
{
};

[[deprecated("bar is unsafe; use foobar instead")]]
int
bar(int n)
{
  return 42 + n - 1;
}

#if __cplusplus > 201103L

//  Deprecate C for C++14 onwards.
class [[deprecated]] C;

//  Deprecate foobar for C++14 onwards.
[[deprecated]]
int
foobar(int n);

#endif

class C
{
};

int
foobar(int n)
{
  return 43 + n - 1;
}

int
main()
{
  A aaa; // { dg-warning "is deprecated" }
  int n = foo(12); // { dg-warning "is deprecated" }

  B bbb; // { dg-warning "is deprecated" "B has been superceded by C" }
  int m = bar(666); // { dg-warning "is deprecated" "bar is unsafe; use foobar instead" }

  C ccc; // { dg-warning "is deprecated" }
  int l = foobar(8); // { dg-warning "is deprecated" }
}
