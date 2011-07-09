/* This testcase generated invalid assembly on IA-32,
   since %gs:0 memory load was not exposed to the compiler
   as memory load and mem to mem moves are not possible
   on IA-32.  */
/* { dg-do link } */
/* { dg-options "-O2 -ftls-model=initial-exec" } */
/* { dg-options "-O2 -ftls-model=initial-exec -march=i686" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-require-effective-target tls } */
/* { dg-require-effective-target tls_runtime  } */

__thread int thr;

struct A
{
  unsigned int a, b, c, d, e;
};

int bar (int x, unsigned long y, void *z)
{
  return 0;
}

int
foo (int x, int y, const struct A *z)
{
  struct A b;
  int d;

  b = *z;
  d = bar (x, y, &b);
  if (d == 0 && y == 0x5402)
    {
      int e = thr;
      d = bar (x, 0x5401, &b);
      if (d)
	{
	  thr = e;
	  d = 0;
	}
      else if ((z->c & 0600) != (b.c & 0600)
	       || ((z->c & 060) && ((z->c & 060) != (b.c & 060))))
	{
	  thr = 22;
	  d = -1;
	}
    }

  return d;
}

int main (void)
{
  foo (1, 2, 0);
  return 0;
}
