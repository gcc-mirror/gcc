/* { dg-do compile } */
/* PR target/120995 ICE unrecognized subword atomic cas */
/* { dg-options "-O" } */
/* { dg-add-options riscv_zacas } */
/* { dg-add-options riscv_zabha } */

_Bool b;
void atomic_bool_cmpxchg()
{
  __sync_bool_compare_and_swap(&b, 1, 0);
}
