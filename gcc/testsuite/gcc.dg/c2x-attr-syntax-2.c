/* Test C2x attribute syntax.  Test ignored attributes diagnosed.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* A GNU attribute that is valid in some contexts, but should be
   diagnosed in contexts where all attributes are ignored (attribute
   declarations, except for fallthrough attributes, and
   statements).  */
#define CTX [[gnu::const]]

/* An attribute that is unknown, so ignored with a warning.  */
#define UNK [[gnu::no_such_attribute(![!(!)!]!,;;)]]

CTX; /* { dg-warning "ignored" } */
UNK; /* { dg-warning "ignored" } */

UNK extern int a; /* { dg-warning "ignored" } */
extern int UNK a; /* { dg-warning "ignored" } */
extern int a UNK; /* { dg-warning "ignored" } */

int f () UNK; /* { dg-warning "ignored" } */
int f (void) UNK; /* { dg-warning "ignored" } */
int g (UNK int a); /* { dg-warning "ignored" } */
int g (int UNK a); /* { dg-warning "ignored" } */
int g (int a UNK); /* { dg-warning "ignored" } */
int g (UNK int); /* { dg-warning "ignored" } */
int g (int UNK); /* { dg-warning "ignored" } */
int g (int) UNK; /* { dg-warning "ignored" } */

int *UNK p; /* { dg-warning "ignored" } */
int b[3] UNK; /* { dg-warning "ignored" } */

int h (int () UNK); /* { dg-warning "ignored" } */

struct UNK s; /* { dg-warning "ignored" } */
union UNK u; /* { dg-warning "ignored" } */

struct UNK s2 { int a; }; /* { dg-warning "ignored" } */
union UNK u2 { int a; }; /* { dg-warning "ignored" } */

struct s3 { UNK int a; }; /* { dg-warning "ignored" } */
struct s4 { int UNK a; }; /* { dg-warning "ignored" } */
union u3 { UNK int a; }; /* { dg-warning "ignored" } */
union u4 { int UNK a; }; /* { dg-warning "ignored" } */

int z = sizeof (int UNK); /* { dg-warning "ignored" } */

enum UNK { E1 }; /* { dg-warning "ignored" } */
enum { E2 UNK }; /* { dg-warning "ignored" } */
enum { E3 UNK = 4 }; /* { dg-warning "ignored" } */

void
func (void) UNK { /* { dg-warning "ignored" } */
  UNK int var; /* { dg-warning "ignored" } */
  CTX { } /* { dg-warning "ignored" } */
  CTX; /* { dg-warning "ignored" } */
  CTX var = 1; /* { dg-warning "ignored" } */
  CTX x: var = 2; /* { dg-warning "ignored" } */
  for (UNK int zz = 1; zz < 10; zz++) ; /* { dg-warning "ignored" } */
}
