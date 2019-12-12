// PR c++/87386
// { dg-do compile { target c++11 } }
// { dg-options "-fdiagnostics-show-caret" }

namespace foo {
  template<typename> struct test { static constexpr bool value = false; };
}
static_assert (foo::test<int>::value, "foo");		// { dg-error "static assertion failed: foo" }
/* { dg-begin-multiline-output "" }
 static_assert (foo::test<int>::value, "foo");
                ~~~~~~~~~~~~~~~~^~~~~
   { dg-end-multiline-output "" } */

static_assert (foo::test<int>::value && true, "bar");	// { dg-error "static assertion failed: bar" }
/* { dg-begin-multiline-output "" }
 static_assert (foo::test<int>::value && true, "bar");
                ~~~~~~~~~~~~~~~~~~~~~~^~~~~~~
   { dg-end-multiline-output "" } */
