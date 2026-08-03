/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-additional-options "-fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef __UINT16_TYPE__ u16;
typedef __UINT8_TYPE__ u8;
typedef __INT16_TYPE__ i16;
typedef __INT8_TYPE__ i8;

static inline u8
clip_u8 (u16 x)
{
  return x & (u16) ~(u16) 255 ? (u8) 255 : (u8) x;
}

/*
** clu:
**	...
**	ldp	q[0-9]+, q[0-9]+, \[x[0-9]+\]
**	uqxtn	v[0-9]+\.8b, v[0-9]+\.8h
**	uqxtn	v[0-9]+\.8b, v[0-9]+\.8h
**	stp	d[0-9]+, d[0-9]+, \[x[0-9]+\]
**	ret
*/
void
clu (u8 *__restrict r, u16 *__restrict x)
{
  for (int i = 0; i < 16; i++)
    r[i] = clip_u8 (x[i]);
}

static inline i8
clip_i8 (i16 x)
{
  i8 t = (i8) x;
  return (i16) -128 <= x && x <= (i16) 127 ? t : x < 0 ? -128 : 127;
}

/*
** cls:
**	...
**	ldp	q[0-9]+, q[0-9]+, \[x[0-9]+\]
**	sqxtn	v[0-9]+\.8b, v[0-9]+\.8h
**	sqxtn	v[0-9]+\.8b, v[0-9]+\.8h
**	stp	d[0-9]+, d[0-9]+, \[x[0-9]+\]
**	ret
*/
void
cls (i8 *__restrict r, i16 *__restrict x)
{
  for (int i = 0; i < 16; i++)
    r[i] = clip_i8 (x[i]);
}

/* The same in variable-length loops, and at the other two element widths.  */
void
clu_n (u8 *__restrict r, u16 *__restrict x, int n)
{
  for (int i = 0; i < n; i++)
    r[i] = clip_u8 (x[i]);
}

void
cls_n (i8 *__restrict r, i16 *__restrict x, int n)
{
  for (int i = 0; i < n; i++)
    r[i] = clip_i8 (x[i]);
}

/* { dg-final { scan-assembler-times {\tuqxtn\tv[0-9]+\.8b, v[0-9]+\.8h} 3 } } */
/* { dg-final { scan-assembler-times {\tsqxtn\tv[0-9]+\.8b, v[0-9]+\.8h} 3 } } */
/* { dg-final { scan-assembler-not {\tuzp1\t} } } */
