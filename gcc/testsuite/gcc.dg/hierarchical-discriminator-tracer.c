/* Test hierarchical discriminators for tracer tail duplication.
   { dg-do compile }
   { dg-options "-O2 -g -ftracer -fno-tree-vectorize -fdump-tree-tracer" } */

volatile int a, b, c;

int
test_tracer (void)
{
  int i;
  for (i = 0; i < 1000; i++)
    {
      if (i % 17)
	a++;
      else
	b++;
      c++;
    }
  return c;
}

/* Superblock formation duplicates the increment of c.  */
/* { dg-final { scan-tree-dump "Duplicated" "tracer" } } */
/* { dg-final { scan-assembler "discriminator (\[1-9\]\[0-9\]*|0x\[1-9a-fA-F\]\[0-9a-fA-F\]*)" { target gas } } } */
