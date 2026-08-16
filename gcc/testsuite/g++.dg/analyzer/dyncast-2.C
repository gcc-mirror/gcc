/* Access control.
   /9.1 constrains only the DST -> SRC path
   /9.2 also requires SRC to be a public base subobject of MDTYPE.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct P1 { virtual ~P1 () {} };
struct P2 { virtual ~P2 () {} };
struct B : private P1 { virtual ~B () {} };
struct C { virtual ~C () {} };
struct U { virtual ~U () {} };
struct MD : B, C, protected P2 { };

void test_nonpublic_src () {
  MD obj;
  P1 *p1 = (P1 *) &obj;
  P2 *p2 = (P2 *) &obj;

  /* /9.1 fails and /9.2's first premise fails.  */
  __analyzer_eval (dynamic_cast<B *> (p1) == NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (dynamic_cast<C *> (p1) == NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (dynamic_cast<C *> (p2) == NULL); /* { dg-warning "TRUE" } */
}

void test_nonpublic_dst () {
  MD obj;
  B *b = &obj;
  /* P2 is a protected, U is not a base.  */
  __analyzer_eval (dynamic_cast<P2 *> (b) == NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (dynamic_cast<U *> (b) == NULL);  /* { dg-warning "TRUE" } */
}

/* Private edge moved across the DST object.  */

struct Base { virtual ~Base () {} };
struct Mid : Base { };
struct Outer : private Mid { };
struct Mid2 : private Base { };
struct Outer2 : Mid2 { };

void test_private_above_dst () {
  Outer obj;
  Base *b = (Base *) (Mid *) &obj;
  /* /9.1 succeeds: Base is a public base of a unique Mid object.
     The private Mid -> Outer edge doesn't affect /9.1.  */
  __analyzer_eval (dynamic_cast<Mid *> (b) != NULL);   /* { dg-warning "TRUE" } */
  /* ... but Outer itself is unreachable.  */
  __analyzer_eval (dynamic_cast<Outer *> (b) == NULL); /* { dg-warning "TRUE" } */
}

void test_private_below_dst () {
  Outer2 obj;
  Base *b = (Base *) (Mid2 *) &obj;
  /* /9.1 fails: no DST object has Base as a public base subobject.  */
  __analyzer_eval (dynamic_cast<Mid2 *> (b) == NULL);	/* { dg-warning "TRUE" } */
  __analyzer_eval (dynamic_cast<Outer2 *> (b) == NULL);	/* { dg-warning "TRUE" } */
}
