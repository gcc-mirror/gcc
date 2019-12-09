/* Test handling of lvalues of incomplete types.  Bugs 36941, 88647
   (invalid), 88827.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

struct S;

extern struct S var;
extern struct S *vp;

void
f8 (void)
{
  /* These are valid because there is no constraint violation and the
     result of '*' is never converted from an lvalue to an rvalue
     (which would yield undefined behavior).  */
  &var;
  &*vp;
  &(var);
  &(*vp);
  &*&*vp;
}
