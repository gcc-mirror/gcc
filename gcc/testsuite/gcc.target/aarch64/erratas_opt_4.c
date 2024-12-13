/* { dg-do link } */
/* { dg-additional-options "-mfix-cortex-a53-835769 -march=armv9-a -###" } */

int main()
{
  return 0;
}

/* { dg-message "-mno-fix-cortex-a53-835769" "note" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */
