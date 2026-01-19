/* Ensure we don't vectorize outer loops when the inner loop is uncounted.  */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */

int a;
int main() {
  for (int b = 0; b < 21; b++) {
    int c = b;
    while (c)
      a = c >>= 1;
  }
  if (a != 0) __builtin_abort();
}

/* { dg-final { scan-tree-dump "missed:   not vectorized: inner-loop count not invariant." "vect" } } */
