/* { dg-do compile { target { musttail && { c || c++11 } } } } */
/* { dg-additional-options "-fdelayed-branch" { target sparc*-*-* } } */

void __attribute__((noipa)) f() {}

void f2()
{
  [[gnu::musttail]] return f2();
}

void f3()
{
  [[gnu::musttail]] return f();
}
