/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_xtheadvector" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadvector" { target { rv64 } } } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo0_1:
**	sb\tzero,0([a-x0-9]+)
**	ret
*/
void foo0_1 (void *p)
{
  __builtin_memset (p, 0, 1);
}

/*
** foo0_7:
**	sb\tzero,0([a-x0-9]+)
**	sb\tzero,1([a-x0-9]+)
**	sb\tzero,2([a-x0-9]+)
**	sb\tzero,3([a-x0-9]+)
**	sb\tzero,4([a-x0-9]+)
**	sb\tzero,5([a-x0-9]+)
**	sb\tzero,6([a-x0-9]+)
**	ret
*/
void foo0_7 (void *p)
{
  __builtin_memset (p, 0, 7);
}

/*
** foo1_1:
**	li\t[a-x0-9]+,1
**	sb\t[a-x0-9]+,0([a-x0-9]+)
**	ret
*/
void foo1_1 (void *p)
{
  __builtin_memset (p, 1, 1);
}

/*
** foo1_5:
**	li\t[a-x0-9]+,1
**	sb\t[a-x0-9]+,0([a-x0-9]+)
**	sb\t[a-x0-9]+,1([a-x0-9]+)
**	sb\t[a-x0-9]+,2([a-x0-9]+)
**	sb\t[a-x0-9]+,3([a-x0-9]+)
**	sb\t[a-x0-9]+,4([a-x0-9]+)
**	ret
*/
void foo1_5 (void *p)
{
  __builtin_memset (p, 1, 5);
}
