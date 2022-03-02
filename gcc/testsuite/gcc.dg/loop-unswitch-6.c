/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -g -fdump-tree-unswitch-details" } */
/* { dg-additional-options "-fcompare-debug" { target { ! powerpc-ibm-aix* } } } */

short a, d;
int b, c;
static int e() {
  int f = -2L, g = 9, h = 0;
  for (; h < 2; h++)
    if (a <= 5) {
      g = 0;
      if (c && a)
        break;
      if (c - 1)
        goto i;
    }
  if (b) {
    int *j[] = {&f};
    if (d)
      for (; f < 9; f++)
        if (g)
          for (; f; f++)
            ;
  i:
    while (f) {
      a--;
      break;
    }
  }
}
int main() { e(); }

/* { dg-final { scan-tree-dump-times "Guard hoisted" 1 "unswitch" } } */
