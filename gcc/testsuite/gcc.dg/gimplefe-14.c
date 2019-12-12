/* { dg-do run } */
/* { dg-options "-O -fgimple" } */

int __GIMPLE (ssa)
main (int argc, char * * argv)
{
  int a;

  __BB(2):
  /* Because of PR82114 we need to handle also 0 as base metal can have
     argc == 0.  */
  switch (argc_2(D)) {default: L2; case 0: L0; case 1: L0; case 2: L1; }

  __BB(3):
L0:
  a_4 = 0;
  goto __BB6;

  __BB(4):
L1:
  a_3 = 3;
  goto __BB6;

  __BB(5):
L2:
  a_5 = -1;
  goto __BB6;

  __BB(6):
  a_1 = __PHI (__BB3: a_4, __BB4: a_3, __BB5: a_5);
  return a_1;

}


