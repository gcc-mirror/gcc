/* Test declaration specifiers.  Test cases that used to be handled in
   a loop in grokdeclarator.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

typedef int t;

/* These should all be diagnosed, but only once, not for every
   identifier declared.  */
struct s0 int x0, /* { dg-error "two or more data types" } */
/* { dg-error "storage size of 'x0' isn't known" "" { target *-*-* } .-1 } */
x1; /* { dg-error "storage size of 'x1' isn't known" } */ 

char union u0 x2, /* { dg-error "two or more data types" } */
x3;

enum e0 struct s1 x4, /* { dg-error "two or more data types" } */
 /* { dg-error "storage size of 'x4' isn't known" "" { target *-*-* } .-1 } */
x5; /* { dg-error "storage size of 'x5' isn't known" } */

short short x6, /* { dg-error "duplicate" } */
x7;

t int x8, /* { dg-error "two or more data types" } */
x9;

long long long x10, /* { dg-error "long long long" } */
x11;
