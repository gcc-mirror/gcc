/* Test that enumerated types are only considered compatible when they
   are the same type.  PR c/6024.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk>, based on
   PR c/6024 from Richard Earnshaw <rearnsha@arm.com> */
/* { dg-do compile } */
/* { dg-options "" } */

/* Original test from PR c/6024.  */
enum e1 {a, b};
enum e2 {c, d};

void f(enum e1); /* { dg-error "prototype" "error at decl" } */

void f(x)
     enum e2 x;
{ /* { dg-error "doesn't match prototype" "error at defn" } */
  return;
}

/* Other compatibility tests.  */
enum e3 { A };
enum e4 { B };

enum e3 v3;
enum e4 *p = &v3; /* { dg-warning "incompatible" "incompatible pointer" } */
enum e3 *q = &v3;

void g(enum e3); /* { dg-error "declaration" "error at first decl" } */
void g(enum e4); /* { dg-error "conflicting types" "error at second decl" } */

void h(enum e3);
void h(enum e3);
