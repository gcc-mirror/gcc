/* { dg-do compile } */
/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-add-options check_function_bodies } */
/* { dg-final { check-function-bodies "**" "" {} } } */

/*
** ang_row:
**	and	w2, w2, 31
**	mov	w[0-9]+, 32
**	sub	w[0-9]+, w[0-9]+, w2
**	dup	v[0-9]+\.16b, w[0-9]+
**	dup	v[0-9]+\.16b, w2
**	movi	v[0-9]+\.8h, 0x10
**	mov	v[0-9]+\.16b, v[0-9]+\.16b
**	ldr	q[0-9]+, \[x1\]
**	ldr	q[0-9]+, \[x1, 1\]
**	umlal	v[0-9]+\.8h, v[0-9]+\.8b, v[0-9]+\.8b
**	umlal	v[0-9]+\.8h, v[0-9]+\.8b, v[0-9]+\.8b
**	umlal2	v[0-9]+\.8h, v[0-9]+\.16b, v[0-9]+\.16b
**	umlal2	v[0-9]+\.8h, v[0-9]+\.16b, v[0-9]+\.16b
**	shrn	v[0-9]+\.8b, v[0-9]+\.8h, 5
**	shrn2	v[0-9]+\.16b, v[0-9]+\.8h, 5
**	str	q[0-9]+, \[x0\]
**	ret
*/
void
ang_row (unsigned char *__restrict d, const unsigned char *__restrict s,
	 int angle)
{
  int f = angle & 31;
  int a = 32 - f;
  for (int x = 0; x < 16; x++)
    d[x] = (unsigned char) ((a * s[x] + f * s[x + 1] + 16) >> 5);
}
