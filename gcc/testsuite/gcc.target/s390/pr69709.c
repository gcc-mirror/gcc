/* PR69709 This testcase used to fail due to a broken risbg
   splitter.  */

/* { dg-do run } */
/* { dg-options "-O3 -march=z10" } */


typedef struct
{
  unsigned int sig[2];
}
val_t;

unsigned int __attribute__ ((noinline))
div_significands (const val_t * a)
{
  val_t u = *a;
  int bit = 64;
  unsigned int r;
  do
    {
      u.sig[1] = (u.sig[1] << 1) | (u.sig[0] >> 31);
      u.sig[0] = 42;

      if (bit == 64)
	r = u.sig[1];
    }
  while (--bit >= 0);
  return r;
}

int
main (void)
{
  val_t a = { { 0x1, 0x1 } };
  if (div_significands (&a) != 2)
    __builtin_abort ();
  return 0;
}
