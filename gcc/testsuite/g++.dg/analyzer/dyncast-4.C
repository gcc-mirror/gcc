/* Virtual bases.  Before C++26, a constexpr constructor/destructor in a
   class with virtual bases is rejected outright (constexpr-dynamic10.C), so
   this part of /9 has no pre-C++26 counterpart in g++.dg/cpp2a.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct A { virtual ~A () {} };

/* One shared V subobject, reached by two paths.  */
struct V { virtual ~V () {} int m; };
struct L : virtual V { };
struct R : virtual V { };
struct Diamond : A, L, R { };

void test_shared_virtual_dst () {
  Diamond obj;
  obj.m = 5;
  A *a = &obj;
  V *v = dynamic_cast<V *> (a);
  __analyzer_eval (v != NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (v->m == 5); /* { dg-warning "TRUE" } */
}

/* Only one of the two paths to the shared V is public.  */
struct LPub : public virtual V { };
struct RPriv : private virtual V { };
struct MixedDiamond : A, LPub, RPriv { };

void test_shared_virtual_dst_mixed_access () {
  MixedDiamond obj;
  A *a = &obj;
  /* [class.access.base]: one public path suffices.  */
  __analyzer_eval (dynamic_cast<V *> (a) != NULL); /* { dg-warning "TRUE" } */
}

/* Two distinct B0 subobjects, each under its own virtual ancestor.  */
struct B0 { virtual ~B0 () {} };
struct V1 : B0 { };
struct V2 : B0 { };
struct D1 : virtual V1 { };
struct D2 : virtual V2 { };
struct TwoB0 : A, D1, D2 { };

void test_distinct_virtual_subobjects () {
  TwoB0 obj;
  A *a = &obj;
  __analyzer_eval (dynamic_cast<B0 *> (a) == NULL); /* { dg-warning "TRUE" } */
}

/* SRC is a shared virtual base with two enclosing C objects: /9.1's
   uniqueness clause fails, and /9.2 then finds C ambiguous.  */
struct S { virtual ~S () {} };
struct C : virtual S { };
struct CL : C { };
struct CR : C { };
struct TwoC : CL, CR { };

void test_virtual_src_two_enclosing_dst () {
  TwoC obj;
  S *s = &obj;
  __analyzer_eval (dynamic_cast<C *> (s) == NULL); /* { dg-warning "TRUE" } */
}

/* Exactly one C derives from the shared S, and it is private in the most
   derived object: /9.1 succeeds where /9.2 cannot.  */
struct OneC : private C { };

void test_virtual_src_one_enclosing_dst () {
  OneC obj;
  S *s = (S *) &obj;
  __analyzer_eval (dynamic_cast<C *> (s) != NULL); /* { dg-warning "TRUE" } */
}

/* As above, but the operand was formed through a sibling that is not a
   C, so the enclosing C is off that path.  */
struct NotC : virtual S { };
struct SideC : NotC, private C { };

void test_virtual_src_dst_off_the_path () {
  SideC obj;
  S *s = (S *) (NotC *) &obj;
  __analyzer_eval (dynamic_cast<C *> (s) != NULL); /* { dg-warning "TRUE" } */
}
