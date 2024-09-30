// { dg-additional-options "-fmodule-header" }

static int foo = 123;
namespace ns {
  using ::foo;  // OK, we're in a header unit
}
