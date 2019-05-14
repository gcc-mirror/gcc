/* { dg-do compile { target { { x86_64-*-* aarch64-*-* ia64-*-* powerpc64-*-* } && lp64 } } } */
/* { dg-options "-Os --param jump-table-max-growth-ratio-for-size=200 --param case-values-threshold=5 -fdump-tree-switchlower1" } */

int a;

int foo(char c) {
  switch (c) {
  case 'c':
    return a;
  case 's':
    return 3;
  case 'n':
    return 1;
  case '%':
    return -2;
  case 'o':
    return a + 2;
    break;
  case 'X':
  case 'x':
    return 2222;
  case 'd':
  case 'i':
  case 'u':
    return 3333;
  default:
    return 0;
  }
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: 37 88 99 100 105 110 111 115 117 120" "switchlower1" } } */
