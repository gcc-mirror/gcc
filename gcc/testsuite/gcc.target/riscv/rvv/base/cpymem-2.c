/* { dg-do compile } */
/* { dg-additional-options "-O1" } */
/* { dg-add-options riscv_v } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef struct { char c[16]; } c16;
typedef struct { char c[32]; } c32;
typedef struct { short s; char c[30]; } s16;

/* A short struct copy can use vsetivli.
** f1:
**	vsetivli\s+zero,16,e8,m1,ta,ma
**	vle8.v\s+v1,0\(a1\)
**	vse8.v\s+v1,0\(a0\)
**	ret
*/
void f1 (c16 *a, c16* b)
{
  *a = *b;
}

/* A longer one needs li.
** f2:
**	li\s+[ta][0-7],32
**	vsetvli\s+zero,[ta][0-7],e8,m2,ta,ma
**	vle8.v\s+v2,0\(a1\)
**	vse8.v\s+v2,0\(a0\)
**	ret
*/
void f2 (c32 *a, c32* b)
{
  *a = *b;
}

/* A 32 byte struct is still short enough for vsetivli
   if we can use an element width larger than 8.
** f3:
**	vsetivli\s+zero,16,e16,m2,ta,ma
**	vle16.v\s+v2,0\(a1\)
**	vse16.v\s+v2,0\(a0\)
**	ret
*/
void f3 (s16 *a, s16* b)
{
  *a = *b;
}
