/* It is a constraint violation for a static function to be declared
   but not defined if it is used except in a sizeof expression whose
   result is an integer constant.  In GNU C, we need to consider
   __typeof__ and __alignof__ as well.  __alignof__ always returns a
   constant, so static functions can always be used therein.
   __typeof__ evaluates its argument iff it has variably modified
   type.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99 -pedantic-errors" } */

/* __alignof__, OK.  */
static int f0(void);
void g0(void) { __alignof__(f0()); }

/* __typeof__ not variably modified, OK.  */
static int f1(void);
void g1(void) { __typeof__(f1()) x; }

/* __typeof__ variably modified, not OK.  */
static int f2(void); /* { dg-error "used but never defined" } */
void g2(void) { __typeof__(int [f2()]) x; }

/* __typeof__ variably modified, not OK.  */
static int f3(void); /* { dg-error "used but never defined" } */
void g3(void) { __typeof__(int (*)[f3()]) x; }

/* Integer sizeof of VM typeof, OK.  */
static int f4(void);
void g4(void) { sizeof(__typeof__(int (*)[f3()])); }
