// PR c++/103049
// P0849R8 - auto(x)
// { dg-do compile { target c++23 } }
// Test invalid use.

void
f ()
{
  char x[] = "foo";
  +decltype(auto){x}; // { dg-error "invalid use of .decltype\\(auto\\)." }
  +decltype(auto)(x); // { dg-error "invalid use of .decltype\\(auto\\)." }

  +auto(); // { dg-error "invalid use of .auto." }
  new auto(); // { dg-error "requires exactly one element" }
  +auto{}; // { dg-error "invalid use of .auto." }
  new auto{}; // { dg-error "requires exactly one element" }
  +auto(1, 2); // { dg-error "invalid use of .auto." }
  new auto(1, 2); // { dg-error "requires exactly one element" }
  +auto{1, 2}; // { dg-error "too many initializers" }
  new auto{1, 2}; // { dg-error "requires exactly one element" }
}
