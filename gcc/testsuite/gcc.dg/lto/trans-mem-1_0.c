/* { dg-lto-options {{-flto -fgnu-tm}} } */
/* { dg-lto-do link } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target fgnu_tm } */

int i;

main()
{
  __transaction_atomic
    {
      i = 0;
    }
}
