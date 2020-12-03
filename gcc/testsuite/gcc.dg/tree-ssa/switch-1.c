/* { dg-do compile { target { { x86_64-*-* aarch64-*-* ia64-*-* powerpc64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fdump-tree-switchlower1 --param case-values-threshold=4" } */

int global;

int foo (int x)
{
  switch (x) {
    case 0:
    case 10:
    case 1000:
    case 1010:
    case 1025 ... 1030:
      return 1;
    default:
      return 0;
  }
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: 0 10 BT:1000-1030" "switchlower1" } } */

int foo2 (int x)
{
  switch (x) {
    case -100:
    case 100:
    case 1000:
    case 10000:
    case 100000:
      return 1;
    default:
      return 0;
  }
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: -100 100 1000 10000 100000" "switchlower1" } } */

int foo3 (int x)
{
  switch (x) {
    case 0:
    case 10:
    case 20:
      global += 1;
      return 3;
    case 30:
    case 33 ... 55:
    case 57:
      return 4;
    case 60 ... 62:
      return 1;
    default:
      return 0;
  }
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: BT:0-62" "switchlower1" } } */

int foo4 (int x)
{
  switch (x) {
    case -100:
    case 10:
    case 20:
      global += 1;
      return 3;
    case 30:
    case 33 ... 55:
    case 57:
      return 4;
    case 60 ... 62:
      return 1;
    case 600 ... 700:
      return 12;
    default:
      return 0;
  }
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: -100 BT:10-62 600-700" "switchlower1" } } */

int foo5 (int x)
{
  switch (x) {
    case 10:
    case 20:
      global += 1;
      return 3;
    case 30:
    case 33 ... 55:
    case 57:
      return 4;
    case 60 ... 62:
      return 1;
    case 600 ... 700:
      return 12;
    case 1000 ... 1010:
    case 1012:
      return 333;
    case 1019:
    case 1021:
      return 9;
    case 111111:
      return 3;
    default:
      return 0;
  }
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: BT:10-62 600-700 JT:1000-1021 111111" "switchlower1" } } */
