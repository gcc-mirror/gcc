/* Version 2.2.0 of MPFR had bugs in sin rounding.  This test checks
   to see if that buggy version was installed.  The problem is fixed
   in version 2.2.1 and presumably later MPFR versions.

   Origin: Kaveh R. Ghazi 10/23/2006.  */

/* { dg-do link } */

extern void link_error (int, double, double);

#define TESTIT(ARG,RES) do { \
  if (sizeof (double) == 8 && __builtin_sin(ARG) != RES) \
    link_error(__LINE__, __builtin_sin(ARG), RES); \
  } while (0);

int main()
{
  TESTIT (-0x1.c0016155c4da3p-1, -0x1.88fc58bcf030dp-1);

  TESTIT (0x1.30654d85c2756p-2, 0x1.2beeb9de27a79p-2);

  TESTIT (0x1.fe68ccaa8e201p+2, 0x1.fc3f0c54e97a7p-1);

  /* This case should always pass.  */
  TESTIT (0.0, 0.0);

  return 0;
}

