/* Header file for pr96530.c test - contains test functions only */

/* PR target/96530 */

/* Verify we do not reject bar() below due to the typedef.  */

typedef __vector_quad vquad_t;

void
foo (__vector_quad *dst)
{
  __builtin_mma_xxsetaccz (dst);
}

void
bar (vquad_t *dst)
{
  __builtin_mma_xxsetaccz (dst);
}
