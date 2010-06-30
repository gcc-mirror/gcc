// { dg-do compile }
// This testcase used to seg fault (PR c++/38648)

// { dg-prune-output "initializer lists" }

char a[1];

int foo(
{
  a = ""; // { dg-error "" }
  return 0; // { dg-error "" }
} // { dg-error "" }
