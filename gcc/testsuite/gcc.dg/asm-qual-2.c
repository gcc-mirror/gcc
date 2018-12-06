/* Test that qualifiers on asm are allowed in any order.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
f (void)
{
  asm volatile goto ("" :::: lab);
  asm goto volatile ("" :::: lab);

  /* Duplicates are not allowed.  */
  asm goto volatile volatile ("" :::: lab);  /* { dg-error "" } */
  asm volatile goto volatile ("" :::: lab);  /* { dg-error "" } */
  asm volatile volatile goto ("" :::: lab);  /* { dg-error "" } */
  asm goto goto volatile ("" :::: lab);  /* { dg-error "" } */
  asm goto volatile goto ("" :::: lab);  /* { dg-error "" } */
  asm volatile goto goto ("" :::: lab);  /* { dg-error "" } */

lab:
  ;
}
