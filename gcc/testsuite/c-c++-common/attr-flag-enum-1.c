/* { dg-additional-options -Wswitch } */

enum E0 { a0 = 1, b0 = 2 };
void f0 (enum E0 e) {
  switch (e) {
  case !(a0|b0):		/* { dg-warning "not in enumerated type" } */
  case a0|b0:			/* { dg-warning "not in enumerated type" } */
  default:;
  }
}

enum __attribute ((flag_enum)) E1 { a1 = 1, b1 = 2 };
void f1 (enum E1 e) {
  switch (e) {
  case !(a1|b1):		/* { dg-bogus "not in enumerated type" } */
  case a1|b1:			/* { dg-bogus "not in enumerated type" } */
  default:;
  }
}

enum [[gnu::flag_enum]] E2 { a2 = 1, b2 = 2 };
void f2 (enum E2 e) {
  switch (e) {
  case !(a2|b2):		/* { dg-bogus "not in enumerated type" } */
  case a2|b2:			/* { dg-bogus "not in enumerated type" } */
  default:;
  }
}

enum [[clang::flag_enum]] E3 { a3 = 1, b3 = 2 };
void f3 (enum E3 e) {
  switch (e) {
  case !(a3|b3):		/* { dg-bogus "not in enumerated type" } */
  case a3|b3:			/* { dg-bogus "not in enumerated type" } */
  default:;
  }
}
