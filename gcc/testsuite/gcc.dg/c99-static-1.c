/* It is a constraint violation for a static function to be declared
   but not defined if it is used except in a sizeof expression whose
   result is an integer constant.  The use of the function simply
   being unevaluated is not enough.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-O2 -std=iso9899:1999 -pedantic-errors" } */

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

/* OK (VM type, not VLA).  */
static int f4(void);
void g4(void) { sizeof(int (*)[f4()]); }

/* Constraint violation (VLA).  */
static int f5(void); /* { dg-error "used but never defined" "VLA" { xfail *-*-* } } */
void g5(void) { sizeof(int [0 ? f5() : 1]); }

/* OK (non-constant sizeof inside constant sizeof).  */
static int f6(void);
void g6(void) { sizeof(sizeof(int [f6()])); }
