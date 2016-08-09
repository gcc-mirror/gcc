/* { dg-options "-O0 -fdump-ipa-profile" } */

int
main (int argc, char **argv)
{
  unsigned u = (argc - 1);
  int counter = 0;

  for (unsigned i = 0; i < 100; i++)
  {
    unsigned x = i < 10 ? 16 : 15;
    counter += u % x;
  }

  return counter;
}

/* autofdo does not do value profiling so far */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Pow2 counter pow2:10 nonpow2:90." "profile" } } */
