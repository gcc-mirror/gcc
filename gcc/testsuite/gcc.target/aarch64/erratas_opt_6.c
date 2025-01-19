/* { dg-do link } */
/* { dg-additional-options "-march=armv8-a -mfix-cortex-a53-835769 -###" } */

int main()
{
  return 0;
}

/* { dg-message "-mfix-cortex-a53-835769" "note" { target *-*-* } 0 } */
/* { dg-message "--fix-cortex-a53-835769" "note" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */
