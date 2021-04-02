// PR c++/68722

class A {
  &__loc   // { dg-error "" }
} class ios_base {  // { dg-error "" }
  A _M_ios_locale ios_base(ios_base &) template <_Traits> class basic_ios {  // { dg-error "" }
    basic_ios basic_ios = operator= // { dg-error "" }
// { dg-prune-output "file ends in default argument" }
