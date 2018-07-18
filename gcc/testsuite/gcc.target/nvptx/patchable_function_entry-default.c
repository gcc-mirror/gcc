/* { dg-do compile } */
/* { dg-options "-O2 -fpatchable-function-entry=3,1" } */

extern int a;

int f3 (void);

int
__attribute__((noinline))
f3 (void)
{
  return 5*a;
}

/* { dg-excess-errors "sorry, unimplemented: not generating patch area, nops not supported" } */
