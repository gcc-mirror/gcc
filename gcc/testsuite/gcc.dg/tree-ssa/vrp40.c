/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int f(int a) {
    switch (a & 1) {
      case 0:
      case 1: return  3;
      case 2: return  5;
      case 3: return  7;
      case 4: return 11;
      case 5: return 13;
      case 6: return 17;
      case 7: return 19;
    }
}

/* { dg-final { scan-tree-dump "return 3;" "vrp1" } } */
