/* { dg-do compile } */
/* { dg-options "-O2 -mno-long-calls" }  */

extern int bar (void *);

int
foo (void)
{
  return bar ((void*)bar);
}

/* { dg-final { scan-assembler "bl?\\s+bar" } } */
