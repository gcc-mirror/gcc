/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -ftree-vectorize -funswitch-loops" } */

static int ready[10];
void abort (void);
void test_once (int t,int t1)
{
  int i, repeat;
  for (i = 0; i < 10; i++)
    {
      ready[i] = 0;
      if (t1)
	if (b())
	  abort ();
    }
  if (t)
    abort ();
}
