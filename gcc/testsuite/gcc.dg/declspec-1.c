/* Test declaration specifiers.  Test cases that used to be handled in
   a loop in grokdeclarator.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

typedef int t;

/* These should all be diagnosed, but only once, not for every
   identifier declared.  */
struct s0 int x0, /* { dg-error "two or more data types" } */
x1;

char union u0 x2, /* { dg-error "two or more data types" } */
x3;

enum e0 struct s1 x4, /* { dg-error "two or more data types" } */
x5;

short short x6, /* { dg-error "duplicate" } */
x7;

t int x8, /* { dg-error "two or more data types" } */
x9;

long long long x10, /* { dg-error "long long long" } */
x11;
