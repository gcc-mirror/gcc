// { dg-do compile }
// Contributed by: Michael Elizabeth Chastain 
//   <mec dot gnu at mindspring dot com>
// PR c++/13927: Wrong error message for redeclartion of type from union

void foo(void)
{
  union { int alpha; int beta; }; // { dg-error "previous declaration of 'int alpha'" }
  double alpha;  // { dg-error "redeclared" }
}

// This checks both the templated version, and the position of the diagnostic
//  (which is currently wrong).
template <int>
void tfoo(void)
{
  union { 
    int alpha;  // { dg-error "" "" { xfail *-*-* } }
    int beta; 
  }; // { dg-bogus "" "misplaced position of the declaration" { xfail *-*-* } }
  double alpha; // { dg-error "" "" }
}

// The duplicated error messages are xfailed for now (tracked in the PR)
// { dg-bogus "" "duplicate error messages" { target *-*-* } 8 }
// { dg-bogus "" "duplicate error messages" { target *-*-* } 9 }
