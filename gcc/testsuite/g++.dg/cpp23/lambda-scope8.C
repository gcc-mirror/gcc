// P2036R3 - Change scope of lambda trailing-return-type
// PR c++/102610
// { dg-do compile { target c++23 } }
// { dg-options "-Wshadow" }

void
bad ()
{
  [x=1](int x){};  // { dg-error "declared as a capture" }
  // { dg-warning "shadows a lambda capture" "" { target *-*-* } .-1 }
  [x=1]{ int x; };  // { dg-error "shadows a parameter" }

  auto f = [i = 5] () { int i; return 0; }; // { dg-error "shadows a parameter" }
  auto f2 = [i = 5] <int N> () { int i; return 0; };  // { dg-error "shadows a parameter" }

  // [expr.prim.lambda.capture]/5
  int x = 0;
  auto g = [x](int x) { return 0; };  // { dg-error "declared as a capture" }
  // { dg-warning "shadows a lambda capture" "" { target *-*-* } .-1 }
  auto h = [y = 0]<typename y>(y) { return 0; };  // { dg-error "shadows template parameter" }

  auto l2 = [i = 0, j = i]() -> decltype(i) { // { dg-error "not declared in this scope" }
    return i;
  };
}
