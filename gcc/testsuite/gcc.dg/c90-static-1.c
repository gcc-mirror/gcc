/* It is a constraint violation for a static function to be declared
   but not defined if it is used except in a sizeof expression.  The
   use of the function simply being unevaluated is not enough.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-O2 -std=iso9899:1990 -pedantic-errors" } */

/* Constraint violation (trivial case, where function is used).  */
static void f0(void); /* { dg-error "used but never defined" } */
void g0(void) { f0(); }

/* Constraint violation.  */
static void f1(void); /* { dg-error "used but never defined" } */
void g1(void) { if (0) { f1(); } }

/* Constraint violation.  */
static int f2(void); /* { dg-error "used but never defined" } */
void g2(void) { 0 ? f2() : 0; }

/* OK.  */
static int f3(void);
void g3(void) { sizeof(f3()); }
