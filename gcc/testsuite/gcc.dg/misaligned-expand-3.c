/* Test that expand can generate correct stores to misaligned data of complex
   type even on strict alignment platforms.  */

/* { dg-do run } */
/* { dg-options "-O0" } */

extern void abort ();

typedef _Complex float mycmplx __attribute__((aligned(1)));

void
foo (mycmplx *p, float r, float i)
{
  __real__ *p = r;
  __imag__ *p = i;
}

#define cvr 3.2f
#define cvi 2.5f
#define NUM 8

struct blah
{
  char c;
  mycmplx x[NUM];
} __attribute__((packed));

struct blah g;

int
main (int argc, char **argv)
{
  int k;

  for (k = 0; k < NUM; k++)
    {
      foo (&g.x[k], cvr, cvi);
      if (__real__ g.x[k] != cvr
	  || __imag__ g.x[k] != cvi)
	abort ();
    }
  return 0;
}
