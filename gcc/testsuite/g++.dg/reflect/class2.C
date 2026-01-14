// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test using a splice expression in an explicit destructor call.

struct S { };
struct X { };

void
ok (S s)
{
  /* [class.dtor]/16:
     In an explicit destructor call, the destructor is specified by
     a ~ followed by a type-name or computed-type-specifier that
     denotes the destructor's class type.  */
  s.~typename [:^^S:]();
}

void
bad (S s)
{
  /* [expr.prim.splice]/2.1.2
     For a splice-expression of the form splice-specifier, let S be
     the construct designated by splice-specifier. The expression is
     ill-formed if S is [...] a destructor  */
  s.~[:^^S:](); // { dg-error "expected" }
  s.~typename [:^^X:](); // { dg-error "the destructor refers to .X." }
}
