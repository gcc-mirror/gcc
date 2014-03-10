// { dg-do compile { target c++11_only } }

class [[deprecated]] A // { dg-warning "attribute directive ignored" }
{
};

[[deprecated]]
int
foo(int n) // { dg-warning "attribute directive ignored" }
{
  return 42 + n;
}

class [[deprecated("B has been superceded by C")]] B // { dg-warning "attribute directive ignored" }
{
};

[[deprecated("bar is unsafe; use foobar instead")]]
int
bar(int n) // { dg-warning "attribute directive ignored" }
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
  A aaa;
  int n = foo(12);

  B bbb;
  int m = bar(666);

  C ccc;
  int l = foobar(8);
}
