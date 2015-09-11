/* PR c/66341 */
/* { dg-do compile } */

void
foo (int *p)
{
  p = 0;
  /* A cast does not yield an lvalue.  */
  (int *) p = 0; /* { dg-error "lvalue required as left operand of assignment" } */
  /* A cast to a qualified type has the same effect as a cast
     to the unqualified version of the type.  */
  (int *const) p = 0; /* { dg-error "lvalue required as left operand of assignment" } */
  (int *) (char *) p = 0; /* { dg-error "lvalue required as left operand of assignment" } */
  (int *) (char *) (int *) p = 0; /* { dg-error "lvalue required as left operand of assignment" } */
  (int *) (char *) (int *) (char *) p = 0; /* { dg-error "lvalue required as left operand of assignment" } */
  (int *) (double *) p = 0; /* { dg-error "lvalue required as left operand of assignment" } */
  (int *) (int *) p = 0; /* { dg-error "lvalue required as left operand of assignment" } */
  (int *) (int *const) p = 0; /* { dg-error "lvalue required as left operand of assignment" } */
}
