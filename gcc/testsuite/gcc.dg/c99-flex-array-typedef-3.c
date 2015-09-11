/* Test for flexible array members.  Test for where structures with
   such members may not occur.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

typedef int T[];
struct flex { int a; T b; };
union rf1 { struct flex a; int b; };
union rf2 { int a; struct flex b; };
union rf3 { int a; union rf1 b; };
union rf4 { union rf2 a; int b; };

/* The above structure and unions may not be members of structures or
   elements of arrays (6.7.2.1#2).  */

struct t0 { struct flex a; }; /* { dg-error "invalid use of structure" } */
struct t1 { union rf1 a; }; /* { dg-error "invalid use of structure" } */
struct t2 { union rf2 a; }; /* { dg-error "invalid use of structure" } */
struct t3 { union rf3 a; }; /* { dg-error "invalid use of structure" } */
struct t4 { union rf4 a; }; /* { dg-error "invalid use of structure" } */

void f0 (struct flex[]); /* { dg-error "invalid use of structure" } */
void f1 (union rf1[]); /* { dg-error "invalid use of structure" } */
void f2 (union rf2[]); /* { dg-error "invalid use of structure" } */
void f3 (union rf3[]); /* { dg-error "invalid use of structure" } */
void f4 (union rf4[]); /* { dg-error "invalid use of structure" } */

struct flex a0[1]; /* { dg-error "invalid use of structure" } */
union rf1 a1[1]; /* { dg-error "invalid use of structure" } */
union rf2 a2[1]; /* { dg-error "invalid use of structure" } */
union rf3 a3[1]; /* { dg-error "invalid use of structure" } */
union rf4 a4[1]; /* { dg-error "invalid use of structure" } */
