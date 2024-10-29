/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* PR target/117346 */
/* Make sure going through ccmp uses similar to non ccmp-case. */
/* This is similar to imm_choice_comparison.c's check except to force
   the use of ccmp by reording the comparison and putting the cast before. */

/*
** check:
**	...
**	mov	w[0-9]+, -16777217
**	...
*/

int
check (int x, int y)
{
  unsigned xu = x;
  if (xu > 0xfefffffe && x > y)
    return 100;

  return x;
}

/*
** check1:
**	...
**	mov	w[0-9]+, -16777217
**	...
*/

int
check1 (int x, int y)
{
  unsigned xu = x;
  if (x > y && xu > 0xfefffffe)
    return 100;

  return x;
}
