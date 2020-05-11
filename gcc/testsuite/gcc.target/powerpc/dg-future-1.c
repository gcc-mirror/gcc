/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_future_hw } */
/* { dg-options "-mdejagnu-cpu=future" } */

/* This tests that powerpc_future_hw works.  */

extern void abort (void);

int futurity (void) {
  long int e = -1;
  asm ("pli %0,%1": "+r" (e) : "n" (0x12345));
  return (e == 0x12345);
}

int main (int argc, char *argv [])
{
  if (!futurity ())
    abort ();
}
