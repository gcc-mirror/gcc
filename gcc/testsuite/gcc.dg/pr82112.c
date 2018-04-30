/* PR target/82112 */
/* { dg-do compile } */
/* { dg-options "-std=gnu90" } */

struct S { int a[10]; } bar (void);
int b, c;

void
foo (void)
{
  __atomic_load (bar ().a, &b, __ATOMIC_ACQUIRE);	/* { dg-error "argument 1 of .__atomic_load. must be a non-void pointer type" } */
  __atomic_load (&b, bar ().a, __ATOMIC_ACQUIRE);	/* { dg-error "argument 2 of .__atomic_load. must be a pointer type" } */
  __atomic_store (bar ().a, &b, __ATOMIC_SEQ_CST);	/* { dg-error "argument 1 of .__atomic_store. must be a non-void pointer type" } */
  __atomic_store (&b, bar ().a, __ATOMIC_SEQ_CST);	/* { dg-error "argument 2 of .__atomic_store. must be a pointer type" } */
  __atomic_exchange (bar ().a, &b, &c, __ATOMIC_RELAXED);	/* { dg-error "argument 1 of .__atomic_exchange. must be a non-void pointer type" } */
  __atomic_exchange (&b, bar ().a, &c, __ATOMIC_RELAXED);	/* { dg-error "argument 2 of .__atomic_exchange. must be a pointer type" } */
  __atomic_exchange (&b, &c, bar ().a, __ATOMIC_RELAXED);	/* { dg-error "argument 3 of .__atomic_exchange. must be a pointer type" } */
  __atomic_compare_exchange (bar ().a, &b, &c, 1, __ATOMIC_RELAXED, __ATOMIC_RELAXED);	/* { dg-error "argument 1 of .__atomic_compare_exchange. must be a non-void pointer type" } */
  __atomic_compare_exchange (&b, bar ().a, &c, 1, __ATOMIC_RELAXED, __ATOMIC_RELAXED);	/* { dg-error "argument 2 of .__atomic_compare_exchange. must be a pointer type" } */
  __atomic_compare_exchange (&b, &c, bar ().a, 1, __ATOMIC_RELAXED, __ATOMIC_RELAXED);	/* { dg-error "argument 3 of .__atomic_compare_exchange. must be a pointer type" } */
}
