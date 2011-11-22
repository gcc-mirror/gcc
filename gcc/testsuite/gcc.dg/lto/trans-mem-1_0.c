/* { dg-lto-options {{-flto -fgnu-tm}} } */

int i;

main()
{
  __transaction_atomic
    {
      i = 0;
    }
}
