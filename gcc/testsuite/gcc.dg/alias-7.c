/* { dg-do run } */
/* { dg-require-alias "" } */
/* { dg-options "-O2" } */

extern void abort (void);

#define ASMNAME(cname)  ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) STRING (prefix) cname
#define STRING(x)    #x

int foo __asm__ (ASMNAME ("foo")) __attribute__((nocommon));
extern __typeof (foo) bar __attribute__ ((weak, alias ("foo")));

int
main (void)
{
  if (&foo != &bar || foo || bar)
    abort ();
  return bar;
}
