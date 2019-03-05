/* Test that qualifiers on asm are allowed in any order.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
f (void)
{
  asm volatile goto ("" :::: lab);
  asm volatile inline ("" :::);
  asm inline volatile ("" :::);
  asm inline goto ("" :::: lab);
  asm goto volatile ("" :::: lab);
  asm goto inline ("" :::: lab);

  asm volatile inline goto ("" :::: lab);
  asm volatile goto inline ("" :::: lab);
  asm inline volatile goto ("" :::: lab);
  asm inline goto volatile ("" :::: lab);
  asm goto volatile inline ("" :::: lab);
  asm goto inline volatile ("" :::: lab);

  /* Duplicates are not allowed.  */
  asm goto volatile volatile ("" :::: lab);  /* { dg-error "" } */
  asm volatile goto volatile ("" :::: lab);  /* { dg-error "" } */
  asm volatile volatile goto ("" :::: lab);  /* { dg-error "" } */
  asm goto goto volatile ("" :::: lab);  /* { dg-error "" } */
  asm goto volatile goto ("" :::: lab);  /* { dg-error "" } */
  asm volatile goto goto ("" :::: lab);  /* { dg-error "" } */

  asm inline volatile volatile ("" :::);  /* { dg-error "" } */
  asm volatile inline volatile ("" :::);  /* { dg-error "" } */
  asm volatile volatile inline ("" :::);  /* { dg-error "" } */
  asm inline inline volatile ("" :::);  /* { dg-error "" } */
  asm inline volatile inline ("" :::);  /* { dg-error "" } */
  asm volatile inline inline ("" :::);  /* { dg-error "" } */

  asm goto inline inline ("" :::: lab);  /* { dg-error "" } */
  asm inline goto inline ("" :::: lab);  /* { dg-error "" } */
  asm inline inline goto ("" :::: lab);  /* { dg-error "" } */
  asm goto goto inline ("" :::: lab);  /* { dg-error "" } */
  asm goto inline goto ("" :::: lab);  /* { dg-error "" } */
  asm inline goto goto ("" :::: lab);  /* { dg-error "" } */

lab:
  ;
}
