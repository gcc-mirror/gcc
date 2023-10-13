/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** f1:
**	...
**	mov	x0, -72340172838076674
**	movk	x0, 0xfeff, lsl 0
**	movk	x0, 0x75fe, lsl 48
**	...
*/

long f1 (void)
{
  return 0x75fefefefefefeff;
}

/*
** f2:
**	...
**	mov	x0, -6148914691236517206
**	movk	x0, 0x5678, lsl 32
**	movk	x0, 0x1234, lsl 48
**	...
*/

long f2 (void)
{
  return 0x12345678aaaaaaaa;
}

/*
** f3:
**	...
**	mov	x0, -3689348814741910324
**	movk	x0, 0x5678, lsl 0
**	movk	x0, 0x1234, lsl 48
**	...
*/

long f3 (void)
{
  return 0x1234cccccccc5678;
}

/*
** f4:
**	...
**	mov	x0, 8608480567731124087
**	movk	x0, 0x5678, lsl 16
**	movk	x0, 0x1234, lsl 32
**	...
*/

long f4 (void)
{
  return 0x7777123456787777;
}

/*
** f5:
**	...
**	mov	x0, 6148914691236517205
**	movk	x0, 0x5678, lsl 0
**	movk	x0, 0x1234, lsl 16
**	...
*/

long f5 (void)
{
  return 0x5555555512345678;
}

/*
** f6:
**	...
**	mov	x0, -4919131752989213765
**	movk	x0, 0x5678, lsl 16
**	movk	x0, 0x1234, lsl 48
**	...
*/

long f6 (void)
{
  return 0x1234bbbb5678bbbb;
}

/*
** f7:
**	...
**	mov	x0, 4919131752989213764
**	movk	x0, 0x5678, lsl 0
**	movk	x0, 0x1234, lsl 32
**	...
*/

long f7 (void)
{
  return 0x4444123444445678;
}
