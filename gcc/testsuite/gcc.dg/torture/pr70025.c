/* PR middle-end/70025 */
/* { dg-do run } */
/* { dg-additional-options "-mtune=z10" { target s390*-*-* } } */
/* { dg-require-effective-target int32plus } */

typedef char (*F) (unsigned long, void *);
typedef union { struct A { char a1, a2, a3, a4; unsigned long a5; F a6; void *a7; } b; char c[1]; } B;
struct C { const char *c1; unsigned long c2; };
typedef struct D { unsigned long d1; int d2; const char *d3; unsigned long d4, d5; struct C d6[49]; char d7[8]; } E[1];

__attribute__ ((noinline, noclone))
void foo (register E p)
{
  asm volatile ("" : : "r" (p) : "memory");
}

__attribute__ ((noinline, noclone))
void bar (register E p)
{
  register unsigned long k = p[0].d1 + 1;
  register struct C *l = &p[0].d6[p[0].d2];
  register const char *m = l->c1;
  p[0].d1 = k;
  if (*m == '\0')
    {
      register struct A *f = &((B *) m)->b;
      register unsigned long n = l->c2;
      register unsigned long o = n + f->a5;
      if (k < o)
	{
	  register unsigned long i;
	  register unsigned long q = k + 8;
	  register F a6 = f->a6;
	  register void *a7 = f->a7;
	  if (q > o)
	    q = o;
	  for (i = k; i < q; i++)
	    p[0].d7[i - k] = (*a6) (i - n, a7);
	  p[0].d4 = k;
	  p[0].d3 = p[0].d7;
	  p[0].d5 = q;
	  return;
	}
    }
  while (p[0].d2 > 0 && l[0].c2 != l[-1].c2)
    {
      p[0].d2--;
      l--;
    }
  if (p[0].d2 == 0)
    {
      p[0].d2 = 0x55555555;
      return;
    }
  p[0].d2--;
  foo (p);
}

char
baz (unsigned long i, void *j)
{
  if (j != 0)
    __builtin_abort ();
  return (char) i;
}

int
main ()
{
  struct D p;
  struct A f;
  __builtin_memset (&f, 0, sizeof (f));
  f.a2 = 4;
  f.a5 = 13;
  f.a6 = baz;
  __builtin_memset (&p, 0, sizeof (p));
  p.d6[0].c1 = (const char *) &f;
  bar (&p);
  if (p.d4 != 1 || p.d5 != 9 || p.d3 != p.d7)
    __builtin_abort ();
  return 0;
}

/* At -O3 the loop in bar() is vectorized and results in a (possibly
   unreachable) out-of-bounds store to p.d7[8]:
     _22(D)->d7[8] = _122;
  { dg-prune-output "-Wstringop-overflow" } */
