/* Test diagnosis of nested tag redefinitions.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

struct s0 {
  struct s0 { int a; } x; /* { dg-error "error: nested redefinition of 'struct s0'" } */
};

struct s1 {
  const struct s1 { int b; } x; /* { dg-error "error: nested redefinition of 'struct s1'" } */
};

struct s2 {
  struct s2 { int c; } *x; /* { dg-error "error: nested redefinition of 'struct s2'" } */
};

struct s3 {
  struct s4 {
    struct s5 {
      struct s3 { int a; } **x; /* { dg-error "error: nested redefinition of 'struct s3'" } */
    } y;
  } z;
};

struct s6;
struct s6 { struct s6 *p; };

union u0 {
  union u0 { int c; } *x; /* { dg-error "error: nested redefinition of 'union u0'" } */
};

enum e0 {
  E0 = sizeof(enum e0 { E1 }) /* { dg-error "error: nested redefinition of 'enum e0'" } */
};

enum e1 {
  E2 = sizeof(enum e2 { E2 }), /* { dg-error "error: redeclaration of enumerator 'E2'" } */
  /* { dg-error "previous definition" "previous E2" { target *-*-* } 38 } */
  E3
};

enum e3;
enum e3 { E4 = 0 };
