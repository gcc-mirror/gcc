/* { dg-do run } */
/* { dg-skip-if "" { { i?86-*-* x86_64-*-* } && { ia32 && { ! nonpic } } } } */
/* { dg-skip-if "Not enough registers" { "pdp11-*-*" } } */
/* { dg-options "-O2 -fgcse-after-reload" } */

extern void abort (void);

__attribute__((noinline)) __complex__ float
give_neg1 (void)
{
  __complex__ float res;
  __real__ res = -1.0;
  __imag__ res = 1.0;
  return res;
}

__attribute__((noinline)) __complex__ float
mycacoshf (__complex__ float x)
{
  __complex__ float res;
  res = give_neg1 ();

  /* We have to use the positive branch.  */
  if (__real__ res < 0.0)
    {
      unsigned a,b,c,d,e,f;
      res = -res; 
      asm __volatile__ ("" : "=r" (a), "=r" (b), "=r" (c), "=r" (d), "=r" (e), "=r" (f));
    }
  return res;
}

int main()
{
  __complex__ float res = mycacoshf(1.0);
  if (__imag__ res >= 0.0)
    abort();
  return 0;
}
