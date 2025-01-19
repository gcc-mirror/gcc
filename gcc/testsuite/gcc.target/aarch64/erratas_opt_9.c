/* { dg-do link } */
/* { dg-additional-options "-mfix-cortex-a53-843419 -###" } */

int main()
{
  return 0;
}

/* { dg-message "-mfix-cortex-a53-843419" "note" { target *-*-* } 0 } */
/* { dg-message "--fix-cortex-a53-843419" "note" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */
