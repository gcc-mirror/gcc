enum a { A };
enum a { B }; /* { dg-bogus "nested redefinition" } */
/* { dg-error "redeclaration of 'enum a'" "" { target *-*-* } .-1 } */

enum empty {}; /* { dg-error "empty enum is invalid" } */
enum empty {}; /* { dg-bogus "nested redefinition" } */
/* { dg-error "empty enum is invalid" "" { target *-*-* } .-1 } */

enum nested_first {
  C1 = sizeof(enum nested_first { C1a }), /* { dg-error "nested redefinition of 'enum nested_first" } */
  C2 = sizeof(enum nested_first { C2a }) /* { dg-error "redeclaration of 'enum nested_first'" "" } */
};

enum nested_second {
  D1,
  D2 = sizeof(enum nested_second { D2a }), /* { dg-error "nested redefinition of 'enum nested_second" } */
  D3 = sizeof(enum nested_second { D3a }) /* { dg-error "redeclaration of 'enum nested_second'" "" } */
};

enum nested_repeat { E };
enum nested_repeat { /* { dg-error "redeclaration of 'enum nested_repeat'" "" } */
  F = sizeof(enum nested_repeat { Fa }) /* { dg-error "nested redefinition of 'enum nested_repeat" } */
};

enum nested_empty {
  G1 = sizeof(enum nested_empty {}), /* { dg-error "nested redefinition of 'enum nested_empty" } */
  /* { dg-error "empty enum is invalid" "" { target *-*-* } .-1 } */
  G2 = sizeof(enum nested_empty { G2a })
};
