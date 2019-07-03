/* { dg-do compile { target { { x86_64-*-* aarch64-*-* ia64-*-* powerpc64-*-* } && lp64 } } } */
/* { dg-options "-Os -fdump-tree-switchlower1" } */

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

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: 37 88 JT:99-120" "switchlower1" } } */
