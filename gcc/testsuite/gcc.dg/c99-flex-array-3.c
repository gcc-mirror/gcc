/* Test for flexible array members.  Test for where structures with
   such members may not occur.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct flex { int a; int b[]; };
union rf1 { struct flex a; int b; };
union rf2 { int a; struct flex b; };
union rf3 { int a; union rf1 b; };
union rf4 { union rf2 a; int b; };

/* The above structure and unions may not be members of structures or
   elements of arrays (6.7.2.1#2).  */

struct t0 { struct flex a; }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "struct in struct" { target *-*-* } .-1 } */
struct t1 { union rf1 a; }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "union in struct" { target *-*-* } .-1 } */
struct t2 { union rf2 a; }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "union in struct" { target *-*-* } .-1 } */
struct t3 { union rf3 a; }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "recursive union in struct" { target *-*-* } .-1 } */
struct t4 { union rf4 a; }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "recursive union in struct" { target *-*-* } .-1 } */

void f0 (struct flex[]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "struct in array" { target *-*-* } .-1 } */
void f1 (union rf1[]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "union in array" { target *-*-* } .-1 } */
void f2 (union rf2[]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "union in array" { target *-*-* } .-1 } */
void f3 (union rf3[]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "recursive union in array" { target *-*-* } .-1 } */
void f4 (union rf4[]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "recursive union in array" { target *-*-* } .-1 } */

struct flex a0[1]; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "struct in array" { target *-*-* } .-1 } */
union rf1 a1[1]; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "union in array" { target *-*-* } .-1 } */
union rf2 a2[1]; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "union in array" { target *-*-* } .-1 } */
union rf3 a3[1]; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "recursive union in array" { target *-*-* } .-1 } */
union rf4 a4[1]; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "invalid use of structure" "recursive union in array" { target *-*-* } .-1 } */
