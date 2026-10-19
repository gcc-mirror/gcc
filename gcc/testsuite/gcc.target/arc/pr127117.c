/* { dg-do run } */
/* { dg-require-effective-target norm } */
/* { dg-options "-O2" } */

unsigned __attribute__ ((noipa))
clz (unsigned x)
{
  return __builtin_clz (x);
}

unsigned __attribute__ ((noipa))
ctz (unsigned x)
{
  return __builtin_ctz (x);
}

int
main (void)
{
  unsigned msb, lsb;

  if (clz (0) != 31 || ctz (0) != 31)
    __builtin_abort ();

  for (msb = 0; msb < 32; msb++)
    for (lsb = 0; lsb <= msb; lsb++)
      {
	unsigned v = (1u << msb) | (1u << lsb);

	if (clz (v) != 31 - msb || ctz (v) != lsb)
	  __builtin_abort ();
      }

  return 0;
}
