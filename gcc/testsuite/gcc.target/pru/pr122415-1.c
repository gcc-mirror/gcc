/* { dg-do assemble } */
/* { dg-options "-O1" } */
/* { dg-final { object-size text <= 24 } } */

void test(void)
{
  (*(unsigned *)0x11223344) = 1;
  (*(unsigned *)0x11223364) = 1;
}
