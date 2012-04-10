/* Test that expand can generate correct stores to misaligned data even on
   strict alignment platforms.  */

/* { dg-do run } */
/* { dg-options "-O0" } */

extern void abort ();

typedef unsigned int myint __attribute__((aligned(1)));

void
foo (myint *p, unsigned int i)
{
  *p = i;
}

#define cst (int) 0xdeadbeef
#define NUM 8

struct blah
{
  char c;
  myint i[NUM];
};

struct blah g;

int
main (int argc, char **argv)
{
  int k;

  for (k = 0; k < NUM; k++)
    {
      foo (&g.i[k], cst);
      if (g.i[k] != cst)
	abort ();
    }
  return 0;
}
