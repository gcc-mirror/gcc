/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

struct { int f1; } g1;		/* { dg-warning "C\[+\]\[+\]" } */
static struct { int f2; } g2;
struct s { int f3; } g3;
union { int f4; } g4;		/* { dg-warning "C\[+\]\[+\]" } */
static union { int f5; } g5;
union u { int f6; } g6;
enum { A } g7;			/* { dg-warning "C\[+\]\[+\]" } */
static enum { B } g8;
enum E { C } g9;
