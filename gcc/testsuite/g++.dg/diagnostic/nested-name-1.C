// { dg-options "-fdiagnostics-show-caret" }

namespace N
{
  struct A {};
}

N:A a;  /* { dg-error "nested-name-specifier" }
  { dg-begin-multiline-output "" }
 N:A a;
  ^
  ::
  { dg-end-multiline-output "" } */
