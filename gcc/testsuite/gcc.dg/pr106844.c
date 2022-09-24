/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized --param uninit-control-dep-attempts=1" } */

struct {
  int count;
  int array[8];
} fde_merge_v1;

void
fde_merge_i2() {
  unsigned i1;
  do
    while (i1 && fde_merge_v1.array[i1 - 1]) /* { dg-warning "uninitialized" } */
      i1--;
  while (fde_merge_i2);
}
