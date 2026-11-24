/* { dg-do run } */
/* { dg-additional-options "-fipa-pta" } */
/* { dg-additional-sources "errno-alias-2.c" } */
/* { dg-skip-if "errno is defined in the LTO unit" { *-*-* } { "-flto" } { "" } } */

#ifdef __NO_MATH_ERRNO__
int main (void) { return 0; }
#else
#define ASMNAME(cname) ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) STRING (prefix) cname
#define STRING(x) #x

extern int my_errno;
extern double log (double) __asm__ (ASMNAME ("my_log"));
int x;
volatile int k = 1;

static __attribute__((noinline, noclone)) int
f (int *p, double d)
{
  int e1 = *p;
  double r = log (d);
  int e2 = *p;
  return e1 * 100 + e2 + (int) r;
}

int
main (void)
{
  if (f (k ? &my_errno : &x, -1.0) != 33)
    __builtin_abort ();
  return 0;
}
#endif
