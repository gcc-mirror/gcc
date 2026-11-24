/* { dg-do run } */
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

__attribute__((noipa)) int
f (double d, int k)
{
  int *tab[2] = { &x, &my_errno };
  int *p = tab[k & 1];
  int e1 = *p;
  double r = log (d);
  int e2 = *p;
  return e1 * 100 + e2 + (int) r;
}

int
main (void)
{
  if (f (-1.0, 1) != 33)
    __builtin_abort ();
  return 0;
}
#endif
