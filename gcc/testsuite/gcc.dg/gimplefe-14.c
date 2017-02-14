/* { dg-do run } */
/* { dg-options "-O -fgimple" } */

int __GIMPLE ()
main (int argc, char * * argv)
{
  int a;

  bb_2:
  switch (argc_2(D)) {default: L2; case 1: L0; case 2: L1; }

L0:
  a_4 = 0;
  goto bb_6;

L1:
  a_3 = 3;
  goto bb_6;

L2:
  a_5 = -1;

  bb_6:
  a_1 = __PHI (L0: a_4, L1: a_3, L2: a_5);
  return a_1;

}


