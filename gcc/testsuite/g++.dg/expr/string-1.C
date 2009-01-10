// { dg-do compile }
// This testcase used to seg fault (PR c++/38648)

char a[1];

int foo( // { dg-error "extended initializer lists only available" }
{
  a = ""; // { dg-error "" }
  return 0; // { dg-error "" }
} // { dg-error "" }
