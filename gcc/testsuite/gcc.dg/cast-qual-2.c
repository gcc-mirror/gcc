/* Test whether the -Wcast-qual handles cv-qualified functions correctly.  */
/* { dg-do compile } */
/* { dg-options "-Wcast-qual" } */

typedef int (intfn_t) (int);
typedef void (voidfn_t) (void);

typedef const intfn_t *constfn_t;
typedef volatile voidfn_t *noreturnfn_t;

intfn_t intfn;
const intfn_t constfn;
voidfn_t voidfn;
volatile voidfn_t noreturnfn;

intfn_t *i1 = intfn;
intfn_t *i2 = (intfn_t *) intfn;
intfn_t *i3 = constfn;
intfn_t *i4 = (intfn_t *) constfn; /* { dg-bogus "discards qualifier" } */

constfn_t p1 = intfn; /* { dg-warning "makes qualified function" } */
constfn_t p2 = (constfn_t) intfn; /* { dg-warning "new qualifier" } */
constfn_t p3 = constfn;
constfn_t p4 = (constfn_t) constfn;

voidfn_t *v1 = voidfn;
voidfn_t *v2 = (voidfn_t *) voidfn;
voidfn_t *v3 = noreturnfn;
voidfn_t *v4 = (voidfn_t *) noreturnfn; /* { dg-bogus "discards qualifier" } */

noreturnfn_t n1 = voidfn; /* { dg-warning "makes qualified function" } */
noreturnfn_t n2 = (noreturnfn_t) voidfn; /* { dg-warning "new qualifier" } */
noreturnfn_t n3 = noreturnfn;
noreturnfn_t n4 = (noreturnfn_t) noreturnfn;
