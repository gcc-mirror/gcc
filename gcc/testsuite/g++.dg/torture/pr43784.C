/* { dg-do run } */
/* { dg-options "-fno-tree-sra" } */

struct S {int x, y, makemelarge[5];};
S __attribute__((noinline)) f (S &s) {
    S r;
    r.x = s.y;
    r.y = s.x;
    return r;
}
int __attribute__((noinline)) glob (int a, int b)
{
  S local = { a, b };
  local = f (local);
  return local.y;
}
extern "C" void abort (void);
int main (void)
{
  if (glob (1, 3) != 1)
    abort ();
  return 0;
}

