/* { dg-lto-options {{-flto -fgnu-tm}} } */
/* { dg-lto-do link } */
/* { dg-require-effective-target stdint_types } */

int i;

main()
{
  __transaction_atomic
    {
      i = 0;
    }
}
