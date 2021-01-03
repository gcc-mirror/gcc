/* PR target/96506 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

extern void bar0();
extern void bar1();
extern void bar2();
extern void bar3();

typedef __vector_pair vpair_t;
typedef __vector_quad vquad_t;

/* Verify we flag errors on the following.  */

__vector_pair
foo4 (__vector_pair *src)
{ /* { dg-error "invalid use of MMA type .__vector_pair. as a function return value" } */
  return *src;
}

vpair_t
foo5 (vpair_t *src)
{ /* { dg-error "invalid use of MMA type .__vector_pair. as a function return value" } */
  return *src;
}

__vector_quad
foo6 (__vector_quad *src)
{ /* { dg-error "invalid use of MMA type .__vector_quad. as a function return value" } */
  return *src;
}

vquad_t
foo7 (vquad_t *src)
{ /* { dg-error "invalid use of MMA type .__vector_quad. as a function return value" } */
  return *src;
}
