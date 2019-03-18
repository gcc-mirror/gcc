/* { dg-do compile } */
/* { dg-options "-O2 -mindirect-branch=thunk -fdump-tree-switchlower1" } */

int global;

int 
foo (int x)
{
  switch (x & 7)
    {
      case 0: ; return 1722;
      case 1: global += 1; return 1060;
      case 2: ; return 1990;
      case 3: ; return 1242;
      case 4: ; return 1466;
      case 5: ; return 894;
      case 6: ; return 570;
      case 7: ; return 572;
      default: return 0;
    }
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: 1 2 3 4 5 6 7" "switchlower1" } } */
