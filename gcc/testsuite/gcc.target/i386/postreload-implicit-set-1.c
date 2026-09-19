/* The equality a conditional branch tests holds on one of its edges; reload
   CSE must see it through the flags register, so that re-creating the same
   value in the target block goes away.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target lp64 } */
/* { dg-add-options check_function_bodies } */
/* { dg-final { check-function-bodies "**" "" "" } } */

void f1 (double);
void f2 (int);

/*
** foo:
**	testl	%edi, %edi
**	je	\.L[0-9]+
**	jmp	f1
**	jmp	f2
*/
void
foo (int type, double xx)
{
  if (type)
    f1 (xx);
  else
    f2 (type);
}
