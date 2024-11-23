/* { dg-do compile } */
/* { dg-options "-fgimple" } */

/* PR c/117741 */
/* Make sure after a parsing error we
   don't go into an infinite loop. */

int i;
void __GIMPLE foo() {
  i = ) /* { dg-error "" } */
