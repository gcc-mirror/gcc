/* { dg-do compile } */
/* { dg-options "-mcpu=ampere1" } */

__attribute__ ((__always_inline__))
__attribute__ ((target ("arch=armv8-a+crypto")))
inline int bar()
{
  return 5;
}

int foo()
{
  return bar();
}
