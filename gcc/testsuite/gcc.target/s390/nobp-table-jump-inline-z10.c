/* { dg-do run } */
/* { dg-options "-O3 -march=z10 -mzarch --save-temps -mindirect-branch-jump=thunk-inline -mindirect-branch-table" } */

/* case-values-threshold will be set to 20 by the back-end when jump
   thunk are requested.  */

int __attribute__((noipa)) foo1 (void) { return 1; }
int __attribute__((noipa)) foo2 (void) { return 2; }
int __attribute__((noipa)) foo3 (void) { return 3; }
int __attribute__((noipa)) foo4 (void) { return 4; }
int __attribute__((noipa)) foo5 (void) { return 5; }
int __attribute__((noipa)) foo6 (void) { return 6; }
int __attribute__((noipa)) foo7 (void) { return 7; }
int __attribute__((noipa)) foo8 (void) { return 8; }
int __attribute__((noipa)) foo9 (void) { return 9; }
int __attribute__((noipa)) foo10 (void) { return 10; }
int __attribute__((noipa)) foo11 (void) { return 11; }
int __attribute__((noipa)) foo12 (void) { return 12; }
int __attribute__((noipa)) foo13 (void) { return 13; }
int __attribute__((noipa)) foo14 (void) { return 14; }
int __attribute__((noipa)) foo15 (void) { return 15; }
int __attribute__((noipa)) foo16 (void) { return 16; }
int __attribute__((noipa)) foo17 (void) { return 17; }
int __attribute__((noipa)) foo18 (void) { return 18; }
int __attribute__((noipa)) foo19 (void) { return 19; }
int __attribute__((noipa)) foo20 (void) { return 20; }


int __attribute__((noipa))
bar (int a)
{
  int ret = 0;

  switch (a)
    {
    case 1: ret = foo1 (); break;
    case 2: ret = foo2 (); break;
    case 3: ret = foo3 (); break;
    case 4: ret = foo4 (); break;
    case 5: ret = foo5 (); break;
    case 6: ret = foo6 (); break;
    case 7: ret = foo7 (); break;
    case 8: ret = foo8 (); break;
    case 9: ret = foo9 (); break;
    case 10: ret = foo10 (); break;
    case 11: ret = foo11 (); break;
    case 12: ret = foo12 (); break;
    case 13: ret = foo13 (); break;
    case 14: ret = foo14 (); break;
    case 15: ret = foo15 (); break;
    case 16: ret = foo16 (); break;
    case 17: ret = foo17 (); break;
    case 18: ret = foo18 (); break;
    case 19: ret = foo19 (); break;
    case 20: ret = foo20 (); break;
    default:
      __builtin_abort ();
    }

  return ret;
}

int
main ()
{
  if (bar (3) != 3)
    __builtin_abort ();

  return 0;
}

/* 1 x bar
/* { dg-final { scan-assembler-times "exrl" 1 } } */

/* { dg-final { scan-assembler     "section\t.s390_indirect_jump" } } */
/* { dg-final { scan-assembler-not "section\t.s390_indirect_call" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_fromreg" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_frommem" } } */
