/* PR target/96506 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -std=gnu17" } */

extern void bar0();
extern void bar1();
extern void bar2();
extern void bar3();

typedef __vector_pair vpair_t;
typedef __vector_quad vquad_t;

/* Verify we flag errors on the following.  */

void
foo0 (void)
{
  __vector_pair v;
  bar0 (v); /* { dg-error "invalid use of MMA operand of type .__vector_pair. as a function parameter" } */
}

void
foo1 (void)
{
  vpair_t v;
  bar1 (v); /* { dg-error "invalid use of MMA operand of type .__vector_pair. as a function parameter" } */
}

void
foo2 (void)
{
  __vector_quad v;
  bar2 (v); /* { dg-error "invalid use of MMA operand of type .__vector_quad. as a function parameter" } */
}

void
foo3 (void)
{
  vquad_t v;
  bar3 (v); /* { dg-error "invalid use of MMA operand of type .__vector_quad. as a function parameter" } */
}
