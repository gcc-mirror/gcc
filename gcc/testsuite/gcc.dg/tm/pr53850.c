/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O3" } */

unsigned char pp[100];

void
foo (void)
{
  int i;
  __transaction_atomic
  {
    for (i = 0; i < 100; ++i)
      pp[i] = 0x33;
  }
}
