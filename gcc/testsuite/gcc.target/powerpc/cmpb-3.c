/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power6" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_popcntb_ok } */
/* { dg-options "-mcpu=power6" } */

void abort ();

long long int
do_compare (long long int a, long long int b)
{
  return __builtin_cmpb (a, b);	/* { dg-error "builtin function '__builtin_cmpb' not supported in this compiler configuration" } */
}

void expect (long long int pattern, long long int value)
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
