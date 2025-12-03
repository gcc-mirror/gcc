/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-w -O3 -fno-tree-dominator-opts -fno-code-hoisting -fno-tree-pre -fno-tree-dce" } */

/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */

int a, b;
int main() {
  while (a)
    for (a = 0; a != 1; a--)
      if (b)
        break;
  return 0;
}

