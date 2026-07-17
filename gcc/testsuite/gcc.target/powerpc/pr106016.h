/* Header file for pr106016.c test - contains test functions only */

/* PR target/106016 */

/* Make sure we do not ICE on the following test case.  */

extern void bar (__vector_quad *);

void
foo (__vector_quad *a, __vector_quad *b)
{
  __vector_quad arr[2] = {*a, *b};
  bar (&arr[0]);
}
