/* { dg-do compile } */
/* { dg-options "-Os -fdump-ipa-icf"  } */

int a[2];
static int *b = &a[0], *c = &a[1];

int
main ()
{
  *c = 1;
  *b = 0;

  if (a[1] != 1)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
