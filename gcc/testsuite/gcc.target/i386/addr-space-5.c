/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler "gs:" } } */

#define uintptr_t __SIZE_TYPE__

struct S { int a, b, c; };

extern struct S s;

int ct_state3 (void)
{
  int r;
  r = *((int __seg_gs *) (uintptr_t) &s.c);
  return r;
}
