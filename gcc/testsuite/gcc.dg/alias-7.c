/* { dg-do run } */
/* { dg-require-alias "" } */
/* { dg-options "-O2" } */

extern void abort (void);

int foo __asm__ ("foo") __attribute__((nocommon));
extern __typeof (foo) bar __attribute__ ((weak, alias ("foo")));

int
main (void)
{
  if (&foo != &bar || foo || bar)
    abort ();
  return bar;
}
