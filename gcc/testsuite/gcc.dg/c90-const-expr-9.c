/* Test for constant expressions: __builtin_offsetof allowed in
   integer constant expressions but not traditional offsetof
   expansion.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct s {
  int a;
};

struct t {
  struct s a;
  int b[2];
};

#define old_offsetof(TYPE, MEMBER) ((__SIZE_TYPE__) &((TYPE *)0)->MEMBER)

enum e {
  E1 = old_offsetof (struct s, a), /* { dg-error "constant" } */
  E2 = old_offsetof (struct t, a.a), /* { dg-error "constant" } */
  E3 = old_offsetof (struct t, b[1]), /* { dg-error "constant" } */
  E4 = __builtin_offsetof (struct s, a),
  E5 = __builtin_offsetof (struct t, a.a),
  E6 = __builtin_offsetof (struct t, b[1])
};
