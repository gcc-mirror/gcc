/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32 -mbig-endian -fno-pie -no-pie" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef unsigned int u32;

struct pair
{
  u32 high;
  u32 low;
};

/* Aggregate returns use memory-layout ordering.  On RV32BE the first field is
   returned in a0 and the second field in a1.  */

/*
** make_pair:
**	...
**	li	a0,1
**	li	a1,2
**	...
**	jr	ra
*/
struct pair __attribute__ ((noipa))
make_pair (void)
{
  struct pair x = { 1, 2 };
  return x;
}

extern struct pair get_pair (void);
volatile u32 pair_sink[2];

/* The caller must interpret a0 as high and a1 as low.  */

/*
** consume_pair_return:
**	...
**	call	get_pair(?:@plt)?
**	...
**	sw	a0,0\([a-z][0-9]+\)
**	sw	a1,4\([a-z][0-9]+\)
**	...
*/
void __attribute__ ((noipa))
consume_pair_return (void)
{
  struct pair x = get_pair ();
  pair_sink[0] = x.high;
  pair_sink[1] = x.low;
}
