// PR 14875: When using 'or' keyword, the error message speaks of a '||' token
// { dg-do compile }
// { dg-options "" }
#define CHECK(x)  void ::x
  CHECK (and);      // { dg-error "before .and. token" }
  CHECK (and_eq);   // { dg-error "before .and_eq. token" }
  CHECK (bitand);   // { dg-error "before .bitand. token" }
  CHECK (bitor);    // { dg-error "before .bitor. token" }
  CHECK (compl);    // { dg-error "before .compl. token" }
  CHECK (not);      // { dg-error "before .not. token" }
  CHECK (not_eq);   // { dg-error "before .not_eq. token" }
  CHECK (or);       // { dg-error "before .or. token" }
  CHECK (or_eq);    // { dg-error "before .or_eq. token" }
  CHECK (xor);      // { dg-error "before .xor. token" }
  CHECK (xor_eq);   // { dg-error "before .xor_eq. token" }
#undef CHECK
#define CHECK(x)  int x
  CHECK (<:);     // { dg-error "" }
  CHECK (:>);     // { dg-error "before .:>. token" }
#undef CHECK
#define CHECK(x)  x
  CHECK (<%);     // { dg-error "before .<%. token" }
#undef CHECK
#define CHECK(x)  x x
  CHECK (%>);     // { dg-error "before .%>. token" }
#undef CHECK
#define CHECK(x)  x
  CHECK (%:);     // { dg-error "stray .%:. " }
  CHECK (%:%:);   // { dg-error "stray .%:%:. " }


