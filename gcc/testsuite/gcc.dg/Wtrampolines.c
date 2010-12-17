/* Origin: trampoline-1.c Waldek Hebisch <hebisch@math.uni.wroc.pl> */
/* Ported to test -Wtrampolines Magnus Granberg <zorry@gentoo.org> */

/* { dg-do compile } */
/* { dg-require-effective-target trampolines } */
/* { dg-options "-O2 -Wtrampolines" } */

#ifndef NO_TRAMPOLINES

/* This used to fail on various versions of Solaris 2 because the
   trampoline couldn't be made executable.  */

extern void abort(void);
extern double fabs(double);

void foo (void)
{
  const int correct[1100] = {1, 0, -2, 0, 1, 0, 1, -1, -10, -30, -67};
  int i;

  double x1 (void) {return 1; }
  double x2 (void) {return -1;}
  double x3 (void) {return -1;}
  double x4 (void) {return 1; }
  double x5 (void) {return 0; }

  typedef double pfun(void);

  double a (int k, pfun x1, pfun x2, pfun x3, pfun x4, pfun x5)
  {
    double b (void)  /* { dg-warning "trampoline generated for nested function 'b'" } */
    { 
      k = k - 1;
      return a (k, b, x1, x2, x3, x4 );
    }

    if (k <= 0)
      return x4 () + x5 ();
    else
      return b ();
  }

  for (i=0; i<=10; i++)
  {
    if (fabs(a( i, x1, x2, x3, x4, x5 ) - correct [i]) > 0.1)
      abort();
  }
}
#endif

int main (void)
{
#ifndef NO_TRAMPOLINES
  foo ();
#endif
  return 0;
}
