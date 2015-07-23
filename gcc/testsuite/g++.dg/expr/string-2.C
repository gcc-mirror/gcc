// { dg-do compile }
// This testcase used to seg fault (PR c++/38648)

char a[1];

int foo(a = "") // { dg-error "invalid array assignment" }
{
  return 0;
}

