// { dg-do compile { target c++11_only } }
// { dg-options "-pedantic" }

class [[deprecated]] A // { dg-bogus "'deprecated' is a C..14 feature" }
{
};

[[deprecated]] // { dg-bogus "'deprecated' is a C..14 feature" }
int
foo(int n)
{
  return 42 + n;
}

class [[deprecated("B has been superceded by C")]] B // { dg-bogus "'deprecated' is a C..14 feature" }
{
};

[[deprecated("bar is unsafe; use foobar instead")]] // { dg-bogus "'deprecated' is a C..14 feature" }
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

  B bbb; // { dg-warning "is deprecated" }
  int m = bar(666); // { dg-warning "is deprecated" }

  C ccc; // { dg-warning "is deprecated" "" { target { c++14 } } }
  int l = foobar(8); // { dg-warning "is deprecated" "" { target { c++14 } } }
}
