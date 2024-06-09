/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -mrvv-max-lmul=m8 -mrvv-vector-bits=zvl -O3 -fno-vect-cost-model -fno-tree-loop-distribute-patterns" } */

int a[1024];
int b[1024];

_Bool
fn1 ()
{
  _Bool tem;
  for (int i = 0; i < 1024; ++i)
    {
      tem = !a[i];
      b[i] = tem;
    }
  return tem;
}
