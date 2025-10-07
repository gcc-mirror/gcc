/* Test tentative definitions with incomplete type: OK in C2y with internal
   linkage if completed by the end of the translation unit.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors -Wc23-c2y-compat" } */

struct s1;
struct s2;
union u1;
union u2;

struct s1 v1;
struct s2 v2; /* { dg-error "storage size" } */
int v3[];
int v4[]; /* { dg-warning "assumed to have one element" } */
union u1 v5;
union u2 v6; /* { dg-error "storage size" } */

static struct s1 sv1;
static struct s2 sv2; /* { dg-error "storage size" } */
static int sv3[]; /* { dg-warning "array size missing" } */
static int sv4[]; /* { dg-error "array size missing" } */
/* { dg-warning "array size missing" "warning" { target *-*-* } .-1 } */
static union u1 sv5;
static union u2 sv6; /* { dg-error "storage size" } */

struct s1 { int x; };
union u1 { int y; };
extern int v3[1];
extern int sv3[1];
