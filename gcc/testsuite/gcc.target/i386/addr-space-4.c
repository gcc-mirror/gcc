/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler "gs:" } } */

#define uintptr_t __SIZE_TYPE__

struct S { int a, b, c; };

extern struct S __seg_gs s;

int foo (void)
{
  int r;
  r = s.c;
  return r;
}
