// PR c++/97518
// { dg-do compile { target c++11 } }
// { dg-options "-fdiagnostics-show-caret" }

constexpr bool yes () { return true; }
constexpr bool no () { return false; }
constexpr bool yay = true;
constexpr bool nay = false;

void
bar ()
{
  static_assert (true && true && no(), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert (true && true && no(), "");
                                  ~~^~
   { dg-end-multiline-output "" } */
  static_assert (yay && nay, ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert (yay && nay, "");
                         ^~~
   { dg-end-multiline-output "" } */
  static_assert (yes() && no(), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert (yes() && no(), "");
                           ~~^~
   { dg-end-multiline-output "" } */
  static_assert (no() && yes(), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert (no() && yes(), "");
                  ~~^~
   { dg-end-multiline-output "" } */
  static_assert (no() && no() && yes(), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert (no() && no() && yes(), "");
                  ~~^~
   { dg-end-multiline-output "" } */
  static_assert (yes() && yes() && yes () && no() && yes(), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert (yes() && yes() && yes () && no() && yes(), "");
                                              ~~^~
   { dg-end-multiline-output "" } */
  static_assert (yes() && yes() && yes () && (no() && yes()), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert (yes() && yes() && yes () && (no() && yes()), "");
                                               ~~^~
   { dg-end-multiline-output "" } */
  static_assert ((yes() && no()) && no(), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert ((yes() && no()) && no(), "");
                            ~~^~
   { dg-end-multiline-output "" } */
  static_assert ((yes() && no()) && no(), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert ((yes() && no()) && no(), "");
                            ~~^~
   { dg-end-multiline-output "" } */
  static_assert ((no() || no()) && yes(), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert ((no() || no()) && yes(), "");
                  ~~~~~~^~~~~~~~
   { dg-end-multiline-output "" } */
  static_assert ((yes() || no()) && no(), ""); // { dg-error "static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert ((yes() || no()) && no(), "");
                                     ~~^~
   { dg-end-multiline-output "" } */
}
