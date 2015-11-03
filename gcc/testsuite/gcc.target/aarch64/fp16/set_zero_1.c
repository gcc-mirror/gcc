/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mfp16-format=ieee" { target "arm*-*-*" } } */

extern void abort (void);

__attribute__ ((noinline))
void setfoo (__fp16 *f)
{
  *f = 0.0;
}

int
main (int argc, char **argv)
{
  __fp16 a = 1.0;
  setfoo (&a);
  if (a != 0.0)
    abort ();
  return 0;
}
