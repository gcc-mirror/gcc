/* Test that expand can generate correct loads of misaligned data even on
   strict alignment platforms.  */

/* { dg-do run } */
/* { dg-options "-O0" } */

extern void abort ();

typedef unsigned int myint __attribute__((aligned(1)));

unsigned int
foo (myint *p)
{
  return *p;
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
  int i, k;
  for (k = 0; k < NUM; k++)
    {
      g.i[k] = cst;
      i = foo (&g.i[k]);

      if (i != cst)
	abort ();
    }
  return 0;
}
