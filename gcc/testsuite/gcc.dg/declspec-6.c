/* Test declaration specifiers.  Test empty declarations.  Test with
   -pedantic-errors.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -pedantic-errors" } */

/* If a declaration does not declare a declarator, it must declare a
   tag or the members of an enumeration, and must only contain one
   type specifier.  */

typedef int T;

struct s0;
union u0;
enum e0; /* { dg-error "ISO C forbids forward references" } */
enum { E0 };
enum e1 { E1 };

/* Not declaring anything (pedwarns).  */
struct { int a; }; /* { dg-error "unnamed struct/union that defines no instances" } */
int; /* { dg-error "useless type name in empty declaration" } */
long; /* { dg-error "useless type name in empty declaration" } */
T; /* { dg-error "useless type name in empty declaration" } */
static const; /* { dg-warning "useless storage class specifier in empty declaration" } */
/* { dg-error "empty declaration" "static const" { target *-*-* } .-1 } */
union { long b; }; /* { dg-error "unnamed struct/union that defines no instances" } */

/* Multiple type names (errors).  */
struct s1 int; /* { dg-error "two or more data types in declaration specifiers" } */
char union u1; /* { dg-error "two or more data types in declaration specifiers" } */
/* { dg-error "useless type name in empty declaration" "char union" { target *-*-* } .-1 } */
double enum { E2 }; /* { dg-error "two or more data types in declaration specifiers" } */
/* { dg-error "useless type name in empty declaration" "double enum" { target *-*-* } .-1 } */
T struct s2; /* { dg-error "two or more data types in declaration specifiers" } */
/* { dg-error "useless type name in empty declaration" "T struct" { target *-*-* } .-1 } */
long union u2; /* { dg-error "two or more data types in declaration specifiers" } */
/* { dg-error "useless type name in empty declaration" "empty" { target *-*-* } .-1 } */
struct s3 short; /* { dg-error "two or more data types in declaration specifiers" } */
union u3 signed; /* { dg-error "two or more data types in declaration specifiers" } */
unsigned struct s4; /* { dg-error "two or more data types in declaration specifiers" } */
/* { dg-error "useless type name in empty declaration" "empty" { target *-*-* } .-1 } */
_Complex enum { E3 }; /* { dg-error "two or more data types in declaration specifiers" } */
/* { dg-error "useless type name in empty declaration" "empty" { target *-*-* } .-1 } */
/* { dg-error "ISO C90 does not support complex types" "C90" { target *-*-* } .-2 } */
/* { dg-error "ISO C does not support plain 'complex' meaning 'double complex'" "ISO C" { target *-*-* } .-3 } */
