/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-details"  } */

static short a = 0;
short b = -1;
static unsigned short c = 0;

int
main ()
{
  if (a <= b)
   return 1;

  return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
