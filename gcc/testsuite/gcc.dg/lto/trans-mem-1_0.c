/* { dg-lto-options {{-flto -fgnu-tm}} } */
/* { dg-lto-do link } */

int i;

main()
{
  __transaction_atomic
    {
      i = 0;
    }
}
