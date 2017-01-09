/* { dg-do compile } */
/* { dg-options "-O2" }  */

extern int bar (void *);

int
foo (void)
{
  return bar ((void*)bar);
}

/* { dg-final { scan-assembler "bl?\\s+bar" } } */
