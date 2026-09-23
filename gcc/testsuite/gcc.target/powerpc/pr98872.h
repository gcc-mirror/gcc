/* Header file for pr98872.c test - contains test functions only */

/* PR target/98872 */

/* Verify we do not ICE on the following tests.  */

void
foo (__vector_quad *dst)
{
  __vector_quad acc;
  *dst = acc;
}

void
bar (__vector_pair *dst)
{
  __vector_pair pair;
  *dst = pair;
}
