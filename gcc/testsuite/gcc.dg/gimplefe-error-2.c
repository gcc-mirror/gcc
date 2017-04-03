/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE ()
main (int argc, char * * argv)
{

bb_2:
  switch (a) {default: L2; case 1: L0; case 2: L1; } /* { dg-error "undeclared" } */

L0:
  a = 0;
  goto bb_6;

L1:
  a = 3;
  goto L2;

L2:
  return a;
}
