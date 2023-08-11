/* Test GNU extensions __typeof__ and __typeof_unqual__.  Invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

struct s { int i : 2; } x;
union u { unsigned int j : 1; } y;

__typeof__ (x.i) j; /* { dg-error "applied to a bit-field" } */
__typeof_unqual__ (x.i) j2; /* { dg-error "applied to a bit-field" } */
__typeof (y.j) j3; /* { dg-error "applied to a bit-field" } */
__typeof_unqual (y.j) j4; /* { dg-error "applied to a bit-field" } */

static int ok (void);
static int also_ok (void);
static int not_defined (void); /* { dg-error "used but never defined" } */
static int also_not_defined (void); /* { dg-error "used but never defined" } */

_Noreturn void nf1 (void);
__attribute__((noreturn)) void nf2 (void);
void fg (void) {}
__typeof__ (&nf1) pnf1 = fg; /* { dg-error "qualified function pointer from unqualified" } */
__typeof (&nf2) pnf2 = fg; /* { dg-error "qualified function pointer from unqualified" } */
extern void (*pnf1) (void); /* { dg-error "conflicting types for" } */
extern void (*pnf2) (void); /* { dg-error "conflicting types for" } */
extern __typeof (nf1) *pnf1; /* { dg-error "conflicting types for" } */
extern __typeof (nf1) *pnf2; /* { dg-error "conflicting types for" } */
extern __typeof__ (nf2) *pnf1; /* { dg-error "conflicting types for" } */
extern __typeof__ (nf2) *pnf2; /* { dg-error "conflicting types for" } */
__typeof (*&nf1) fg2, fg2a, fg2b; /* { dg-error "ISO C forbids qualified function types" } */
__typeof__ (*&nf2) fg3, fg3a, fg3b; /* { dg-error "ISO C forbids qualified function types" } */
__typeof (nf1) fg4, fg4a, fg4b;
__typeof__ (nf2) fg5, fg5a, fg5b;

extern void abort (void);

void fg2 (void) {} /* { dg-error "conflicting type qualifiers for" } */
_Noreturn void fg2a (void) { abort (); } /* { dg-error "conflicting type qualifiers for" } */
__attribute__((noreturn)) void fg2b (void) { abort (); } /* { dg-error "conflicting type qualifiers for" } */
void fg3 (void) {} /* { dg-error "conflicting type qualifiers for" } */
_Noreturn void fg3a (void) { abort (); } /* { dg-error "conflicting type qualifiers for" } */
__attribute__((noreturn)) void fg3b (void) { abort (); } /* { dg-error "conflicting type qualifiers for" } */
void fg4 (void) {}
_Noreturn void fg4a (void) { abort (); }
__attribute__((noreturn)) void fg4b (void) { abort (); }
void fg5 (void) {}
_Noreturn void fg5a (void) { abort (); }
__attribute__((noreturn)) void fg5b (void) { abort (); }

void
f (void)
{
  __typeof__ (ok ()) x = 2;
  __typeof_unqual (also_ok ()) y = 2;
  int a[2];
  int (*p)[x] = &a;
  __typeof (p + not_defined ()) q;
  __typeof_unqual__ (p + also_not_defined ()) q2;
}
