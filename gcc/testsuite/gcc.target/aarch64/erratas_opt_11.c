/* { dg-do link } */
/* { dg-additional-options "-march=armv9-a -mfix-cortex-a53-843419 -###" } */

int main()
{
  return 0;
}

/* { dg-message "-mno-fix-cortex-a53-843419" "note" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */
