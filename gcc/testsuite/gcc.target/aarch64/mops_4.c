/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.6-a+mops" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

/*
** copy1:
**	mov	(x[0-9]+), x0
**	cpyfp	\[\1\]!, \[x1\]!, x2!
**	cpyfm	\[\1\]!, \[x1\]!, x2!
**	cpyfe	\[\1\]!, \[x1\]!, x2!
**	str	x0, \[x3\]
**	ret
*/
void
copy1 (int *x, int *y, long z, int **res)
{
  __builtin_memcpy (x, y, z);
  *res = x;
}

/*
** copy2:
**	mov	(x[0-9]+), x1
**	cpyfp	\[x0\]!, \[\1\]!, x2!
**	cpyfm	\[x0\]!, \[\1\]!, x2!
**	cpyfe	\[x0\]!, \[\1\]!, x2!
**	str	x1, \[x3\]
**	ret
*/
void
copy2 (int *x, int *y, long z, int **res)
{
  __builtin_memcpy (x, y, z);
  *res = y;
}

/*
** copy3:
**	mov	(x[0-9]+), x2
**	cpyfp	\[x0\]!, \[x1\]!, \1!
**	cpyfm	\[x0\]!, \[x1\]!, \1!
**	cpyfe	\[x0\]!, \[x1\]!, \1!
**	str	x2, \[x3\]
**	ret
*/
void
copy3 (int *x, int *y, long z, long *res)
{
  __builtin_memcpy (x, y, z);
  *res = z;
}

/*
** move1:
**	mov	(x[0-9]+), x0
**	cpyp	\[\1\]!, \[x1\]!, x2!
**	cpym	\[\1\]!, \[x1\]!, x2!
**	cpye	\[\1\]!, \[x1\]!, x2!
**	str	x0, \[x3\]
**	ret
*/
void
move1 (int *x, int *y, long z, int **res)
{
  __builtin_memmove (x, y, z);
  *res = x;
}

/*
** move2:
**	mov	(x[0-9]+), x1
**	cpyp	\[x0\]!, \[\1\]!, x2!
**	cpym	\[x0\]!, \[\1\]!, x2!
**	cpye	\[x0\]!, \[\1\]!, x2!
**	str	x1, \[x3\]
**	ret
*/
void
move2 (int *x, int *y, long z, int **res)
{
  __builtin_memmove (x, y, z);
  *res = y;
}

/*
** move3:
**	mov	(x[0-9]+), x2
**	cpyp	\[x0\]!, \[x1\]!, \1!
**	cpym	\[x0\]!, \[x1\]!, \1!
**	cpye	\[x0\]!, \[x1\]!, \1!
**	str	x2, \[x3\]
**	ret
*/
void
move3 (int *x, int *y, long z, long *res)
{
  __builtin_memmove (x, y, z);
  *res = z;
}

/*
** set1:
**	mov	(x[0-9]+), x0
**	setp	\[\1\]!, x2!, x1
**	setm	\[\1\]!, x2!, x1
**	sete	\[\1\]!, x2!, x1
**	str	x0, \[x3\]
**	ret
*/
void
set1 (char *x, char y, long z, char **res)
{
  __builtin_memset (x, y, z);
  *res = x;
}

/*
** set2:
**	ldrb	w([0-9]+), \[x1\]
**	setp	\[x0\]!, x2!, x\1
**	setm	\[x0\]!, x2!, x\1
**	sete	\[x0\]!, x2!, x\1
**	strb	w\1, \[x3\]
**	ret
*/
void
set2 (char *x, char *yptr, long z, char *res)
{
  char y = *yptr;
  __builtin_memset (x, y, z);
  *res = y;
}

/*
** set3:
**	mov	(x[0-9]+), x2
**	setp	\[x0\]!, \1!, x1
**	setm	\[x0\]!, \1!, x1
**	sete	\[x0\]!, \1!, x1
**	str	x2, \[x3\]
**	ret
*/
void
set3 (char *x, char y, long z, long *res)
{
  __builtin_memset (x, y, z);
  *res = z;
}

/*
** set4:
**	setp	\[x0\]!, x1!, xzr
**	setm	\[x0\]!, x1!, xzr
**	sete	\[x0\]!, x1!, xzr
**	strb	wzr, \[x2\]
**	ret
*/
void
set4 (char *x, long z, char *res)
{
  __builtin_memset (x, 0, z);
  *res = 0;
}
