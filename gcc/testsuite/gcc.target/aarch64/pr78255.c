/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=tiny" } */

extern int bar (void *);

int
foo (void)
{
  return bar ((void *)bar);
}

/* { dg-final { scan-assembler "b\\s+bar" } } */
