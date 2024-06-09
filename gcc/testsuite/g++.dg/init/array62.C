// PR c++/59465
// { dg-do compile }

struct string {} a[1];
struct pair {
  string s[1];
  pair() : s(a) {} // { dg-error "invalid initializer for array member" }
};

struct S {
  char s[10];
  S() : s("aaa") {}
};

void
g ()
{
  string x[1](a); // { dg-error "array must be initialized" }
}
