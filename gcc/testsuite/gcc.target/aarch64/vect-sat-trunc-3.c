/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-additional-options "-fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef __UINT16_TYPE__ u16;

static inline u16
clip_u16 (int x)
{
  return (unsigned int) x > 65535u ? ((-x) >> 31) : x;
}

/*
** clu:
**	...
**	ldp	q[0-9]+, q[0-9]+, \[x[0-9]+\]
**	orr	v[0-9]+\.4s, #1
**	sqxtun	v[0-9]+\.4h, v[0-9]+\.4s
**	orr	v[0-9]+\.4s, #1
**	sqxtun	v[0-9]+\.4h, v[0-9]+\.4s
**	stp	d[0-9]+, d[0-9]+, \[x[0-9]+\]
**	ret
*/
void
clu (u16 *__restrict r, const int *__restrict x)
{
  for (int i = 0; i < 8; i++)
    r[i] = clip_u16 (x[i] | 1);
}

/* The same in a variable-length loop.  */
void
clu_n (u16 *__restrict r, const int *__restrict x, int n)
{
  for (int i = 0; i < n; i++)
    r[i] = clip_u16 (x[i] | 1);
}

/* { dg-final { scan-assembler-times {\tsqxtun\tv[0-9]+\.4h, v[0-9]+\.4s} 3 } } */
/* { dg-final { scan-assembler-not {\tuqxtn\tv[0-9]+\.4h, v[0-9]+\.4s} } } */
