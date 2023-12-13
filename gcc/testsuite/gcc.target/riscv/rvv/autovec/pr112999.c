/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d --param=riscv-autovec-lmul=m8 --param=riscv-autovec-preference=fixed-vlmax -O3 -fno-vect-cost-model -fno-tree-loop-distribute-patterns" } */

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
