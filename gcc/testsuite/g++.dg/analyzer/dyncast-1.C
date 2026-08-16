/* Basic runtime checks:
   [expr.dynamic.cast]/9.1 and /9.2 over public, non-virtual inheritance.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct A { virtual ~A () {} };
struct B : A { };
struct C : B { int m; };
struct S { virtual ~S () {} };
struct D : C, S { };

/* /9.1: SRC is a public base subobject of a unique DST object.  */
void test_downcast () {
  C obj;
  obj.m = 50;
  A *a = &obj;

  __analyzer_eval (dynamic_cast<B *> (a) != NULL); /* { dg-warning "TRUE" } */

  C *c = dynamic_cast<C *> (a);
  __analyzer_eval (c != NULL); /* { dg-warning "TRUE" } */
  /* The result region must alias the FE's own field accesses.  */
  __analyzer_eval (c->m == 50); /* { dg-warning "TRUE" } */
  __analyzer_eval (c == &obj);	/* { dg-warning "TRUE" } */
}

/* The dynamic type is the SRC type itself: no DST above it.  */
void test_no_derived_object () {
  A obj;
  A *a = &obj;
  __analyzer_eval (dynamic_cast<C *> (a) == NULL); /* { dg-warning "TRUE" } */
}

/* SRC sits at a nonzero offset in MDTYPE.  */
void test_from_secondary_base () {
  D obj;
  obj.m = 7;
  S *s = &obj;
  D *d = dynamic_cast<D *> (s);
  __analyzer_eval (d != NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (d->m == 7); /* { dg-warning "TRUE" } */
}

/* /9.2: A and S are unrelated, both public bases of D.  */
void test_sidecast () {
  D obj;
  A *a = &obj;
  S *s = dynamic_cast<S *> (a);
  __analyzer_eval (s != NULL);	     /* { dg-warning "TRUE" } */
  __analyzer_eval (s == (S *) &obj); /* { dg-warning "TRUE" } */
}

/* Nothing is known about the dynamic type.  */
void test_symbolic (A *p) {
  __analyzer_eval (dynamic_cast<S *> (p) == NULL); /* { dg-warning "UNKNOWN" } */
  /* { dg-warning "TRUE" "" { target *-*-* } .-1 } */
}

/* /6: a null operand yields null with no runtime check.  */
void test_null_operand () {
  A *p = NULL;
  __analyzer_eval (dynamic_cast<S *> (p) == NULL); /* { dg-warning "TRUE" } */
}
