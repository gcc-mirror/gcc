/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_xtheadvector -O2" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadvector -O2" { target { rv64 } } } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo1_16:
**	li\t[a-x0-9]+,16
**	th.vsetvli\tzero,[a-x0-9]+,e8,m1
**	th\.vmv\.v\.i\tv[0-9],1
**	th\.vse\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/

void foo1_16 (void *p)
{
   __builtin_memset (p, 1, 16);
}
