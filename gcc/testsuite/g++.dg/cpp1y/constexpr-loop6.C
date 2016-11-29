// PR c++/65970
// { dg-do compile { target c++14 } }
// { dg-options -fconstexpr-loop-limit=5 }

constexpr int foo() {
  while (true)			// { dg-error "-fconstexpr-loop-limit" }
    ;
  return 0;
}

constexpr int i = foo();	// { dg-message "" }
