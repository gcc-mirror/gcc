/* Ambiguity ignores access, and /9.1 is anchored at one SRC subobject
   so a repeated base need not be ambiguous there.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct A { virtual ~A () {} };
struct C { virtual ~C () {} };
struct P1 : C { };
struct P2 : C { };
struct MDpub : A, P1, P2 { };
struct MDpriv : A, P1, private P2 { };

/* /9.1 fails (A is below no C); /9.2 sees two C subobjects.  */
void test_ambiguous_dst () {
  MDpub obj;
  A *a = &obj;
  __analyzer_eval (dynamic_cast<C *> (a) == NULL); /* { dg-warning "TRUE" } */
}

/* One of the two C subobjects is only reachable privately: ambiguity
   ignores access, so this is still null.  */
void test_ambiguous_dst_mixed_access () {
  MDpriv obj;
  A *a = &obj;
  __analyzer_eval (dynamic_cast<C *> (a) == NULL); /* { dg-warning "TRUE" } */
}

/* The SRC subobject selects which C encloses it.  */
void test_repeated_base_anchored_at_src () {
  MDpub obj;
  C *c = (C *) (P1 *) &obj;

  /* /9.1: exactly one P1, and one MDpub, derive from this C.  */
  __analyzer_eval (dynamic_cast<P1 *> (c) != NULL);    /* { dg-warning "TRUE" } */
  __analyzer_eval (dynamic_cast<MDpub *> (c) != NULL); /* { dg-warning "TRUE" } */

  /* /9.1 fails for P2 (this C is not inside one),
     but /9.2 succeeds: P2 is an unambiguous public base of MDpub.
     Result is the other branch of the hierarchy.  */
  __analyzer_eval (dynamic_cast<P2 *> (c) != NULL);	   /* { dg-warning "TRUE" } */
  __analyzer_eval (dynamic_cast<P2 *> (c) == (P2 *) &obj); /* { dg-warning "TRUE" } */
}

/* Same, with P2 private: /9.2 now fails.  */
void test_repeated_base_private_sibling () {
  MDpriv obj;
  C *c = (C *) (P1 *) &obj;
  __analyzer_eval (dynamic_cast<P1 *> (c) != NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (dynamic_cast<P2 *> (c) == NULL); /* { dg-warning "TRUE" } */
}
