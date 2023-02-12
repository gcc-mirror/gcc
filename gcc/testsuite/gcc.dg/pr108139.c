/* { dg-do compile { target int128 } } */
/* { dg-options "-O1 -ftree-vrp -fdump-tree-vrp" } */

int a, b;
int main() {
  int c;
  if (a > 1)
    a++;
  while (a)
    if (c == b)
      c = a;
  return 0;
}


/* { dg-final { scan-tree-dump-not "Folding predicate" "vrp2" } } */


