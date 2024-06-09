/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp2" } */
/* { dg-require-effective-target int32plus } */

int a, b;
int main() {
  char c = a = 0;
  for (; c != -3; c++) {
    int d = 2;
    d ^= 2 && a;
    b = a == 0 ? d : d / a;
    a = b;
  }
  for (; (1 + 95 << 24) + b + 1 + 686658714L + b - 2297271457;)
    ;
}

/* { dg-final { scan-tree-dump-not "Folding predicate" "vrp2" } } */

