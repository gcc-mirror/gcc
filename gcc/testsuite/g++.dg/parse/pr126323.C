// PR c++/126323

class C {
#pragma GCC novector	// { dg-error "must be inside a function" }
#pragma GCC unroll(0)	// { dg-error "must be inside a function" }
#pragma GCC ivdep	// { dg-error "must be inside a function" }
  for (int i = 0; i < 2; i++)	// { dg-error "expected unqualified-id" }
				// { dg-error "does not name a type" "" { target *-*-* } .-1 }
    ;
};
