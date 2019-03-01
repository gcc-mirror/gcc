/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target dfp_hw } */
/* { dg-options "-mdejagnu-cpu=power6" } */

void abort ();

unsigned long long int
do_compare (unsigned long long int a, unsigned long long int b)
{
  return __builtin_cmpb (a, b);
}

void
expect (unsigned long long int pattern, unsigned long long int value)
{
  if (pattern != value)
    abort ();
}

int
main (int argc, char *argv[])
{
  expect (0xff00000000000000LL,
	  do_compare (0x0123456789abcdefLL, 0x0100000000000000LL));
  expect (0x00ffffffffffffff,
	  do_compare (0x0123456789abcdefLL, 0x0023456789abcdefLL));
  expect (0x00000000000000ff,
	  do_compare (0x00000000000000efLL, 0x0123456789abcdefLL));
}
