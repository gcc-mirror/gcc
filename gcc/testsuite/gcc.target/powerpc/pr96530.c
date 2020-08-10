/* PR target/96530 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

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
