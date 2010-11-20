/* PR debug/46561 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ftree-parallelize-loops=2 -fcompare-debug" } */

extern void abort (void);

void foo (char *c)
{
  int i;
  unsigned x = 0;
  {
    for (i = 0; i < 64; i += 4)
	x = x | *((unsigned *) (&c[i]));
    if (x)
      abort ();
  }
}
