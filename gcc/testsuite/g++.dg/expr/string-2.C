// { dg-do compile }
// This testcase used to seg fault (PR c++/38648)

char a[1];

int foo(a = "") // { dg-error "11:invalid array assignment" }
{
  return 0;
}

