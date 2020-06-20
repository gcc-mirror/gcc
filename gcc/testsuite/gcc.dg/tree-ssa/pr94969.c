/* PR tree-optimization/52267 */
/* { dg-do run } */
/* { dg-options "-O3 -fdump-tree-ldist-details" } */

int a = 0, b = 0, c = 0;
struct S {
  signed m : 7;
  signed e : 2;
};
struct S f[2] = {{0, 0}, {0, 0}};
struct S g = {0, 0};

void __attribute__((noinline))
k()
{
  for (; c <= 1; c++) {
    f[b] = g;
    f[b].e ^= 1;
  }
}
int main()
{
  k();
  if (f[b].e != 1)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "Loop 1 distributed: split to 3 loops" "ldist" } } */
