/* { dg-do link { target natural_alignment_32 } } */
/* { dg-do compile { target { ! natural_alignment_32 } } } */
/* { dg-options "-O3 -fdump-tree-cunrolli-optimized" } */

extern void foo(void);
static int b, f, *a = &b;
int **c = &a;
static void d() {
  int g, h;
  for (f = 0; f < 1; f++) {
    int *i = &b;
    {
      int *j[3], **k = &a;
      for (g = 0; g < 3; g++)
        for (h = 0; h < 1; h++)
          j[g] = &b;
      *k = j[0];
    }
    *c = i;
  }
}
int main() {
  d();
  *a = 0;
  if (**c)
    foo();
  return 0;
}

/* Verify that we unroll the inner loop early even with -O3.  */
/* { dg-final { scan-tree-dump "loop with 1 iterations completely unrolled" "cunrolli" } }  */
/* { dg-final { scan-tree-dump "loop with 3 iterations completely unrolled" "cunrolli" } }  */
