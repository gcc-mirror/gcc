/* { dg-require-effective-target int128 } */
/* { dg-additional-options "--param analyzer-max-svalue-depth=0" } */

void x7 (void)
{
  __int128 z5[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
  };
}
