/* { dg-do compile { target { { x86_64-*-* aarch64-*-* ia64-*-* powerpc64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fdump-tree-switchlower1" } */

int global;

int foo (int x)
{
  switch (x) {
    case 0:
    case 10:
      return 1;
    case 20:
    case 30:
    case 62:
      return 2;
    case 1000:
    case 1010:
    case 1025 ... 1030:
      return 1;
    default:
      return 0;
  }
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: BT:0-62 BT:1000-1030" "switchlower1" } } */
