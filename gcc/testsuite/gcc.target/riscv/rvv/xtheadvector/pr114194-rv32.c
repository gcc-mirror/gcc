/* { dg-do compile { target { { ! riscv_abi_e } && rv32 } } } */
/* { dg-options "-march=rv32gc_xtheadvector -O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo0_1:
**	sb\tzero,0\([a-x0-9]+\)
**	ret
*/
void foo0_1 (void *p)
{
  __builtin_memset (p, 0, 1);
}

/*
** foo0_7:
**	li\t[a-x0-9]+,7
**	th.vsetvli\tzero,[a-x0-9]+,e8,m1
**	th\.vmv\.v\.i\tv[0-9],0
**	th\.vse\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void foo0_7 (void *p)
{
  __builtin_memset (p, 0, 7);
}

/*
** foo1_1:
**	li\t[a-x0-9]+,1
**	sb\t[a-x0-9]+,0\([a-x0-9]+\)
**	ret
*/
void foo1_1 (void *p)
{
  __builtin_memset (p, 1, 1);
}

/*
** foo1_5:
**	li\t[a-x0-9]+,5
**	th.vsetvli\tzero,[a-x0-9]+,e8,m1
**	th\.vmv\.v\.i\tv[0-9],1
**	th\.vse\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/

void foo1_5 (void *p)
{
  __builtin_memset (p, 1, 5);
}
