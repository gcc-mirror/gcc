/* { dg-do compile } */
/* { dg-additional-options "-O2 -std=c99  -fno-unwind-tables -fno-asynchronous-unwind-tables" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

#include <stdbool.h>

void h(void);

/*
** g1:
** 	cbnz	w0, .L[0-9]+
** 	ret
** 	...
*/
void g1(int x)
{
  if (__builtin_expect (x, 0))
    h ();
}

/* 
** g2:
** 	tbnz	x0, 0, .L[0-9]+
** 	ret
** 	...
*/
void g2(int x)
{
  if (__builtin_expect (x & 1, 0))
    h ();
}

/* 
** g3:
** 	tbnz	x0, 3, .L[0-9]+
** 	ret
** 	...
*/
void g3(int x)
{
  if (__builtin_expect (x & 8, 0))
    h ();
}

/* 
** g4:
** 	tbnz	w0, #31, .L[0-9]+
** 	ret
** 	...
*/
void g4(int x)
{
  if (__builtin_expect (x & (1 << 31), 0))
    h ();
}

/* 
** g5:
** 	tst	w0, 255
** 	bne	.L[0-9]+
** 	ret
** 	...
*/
void g5(char x)
{
  if (__builtin_expect (x, 0))
    h ();
}

/* 
** g6:
** 	tbnz	w0, 0, .L[0-9]+
** 	ret
** 	...
*/
void g6(char x)
{
  if (__builtin_expect (x & 1, 0))
    h ();
}

/* 
** g7:
** 	tst	w0, 3
** 	bne	.L[0-9]+
** 	ret
** 	...
*/
void g7(char x)
{
  if (__builtin_expect (x & 3, 0))
    h ();
}

/* 
** g8:
** 	tbnz	w0, 7, .L[0-9]+
** 	ret
** 	...
*/
void g8(char x)
{
  if (__builtin_expect (x & (1 << 7), 0))
    h ();
}

/* 
** g9:
** 	tbnz	w0, 0, .L[0-9]+
** 	ret
** 	...
*/
void g9(bool x)
{
  if (__builtin_expect (x, 0))
    h ();
}

/* 
** g10:
** 	tbnz	w0, 0, .L[0-9]+
** 	ret
** 	...
*/
void g10(bool x)
{
  if (__builtin_expect (x & 1, 0))
    h ();
}

