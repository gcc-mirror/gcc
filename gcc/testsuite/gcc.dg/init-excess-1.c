/* Test for various cases of excess initializers for empty objects:
   bug 21873.  Various versions of GCC ICE, hang or loop repeating
   diagnostics on various of these tests.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

struct s0 { };
struct s1 { int a; };
struct s2 { int a; int b; };

int a0[0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
int a1[0][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
int a2[0][1] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
int a3[1][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
int a4[][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
int a5[][0][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
int a6[][0][1] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
int a7[][1][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */

struct s0 b0[0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s0 b1[0][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s0 b2[0][1] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s0 b3[1][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s0 b4[][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s0 b5[][0][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s0 b6[][0][1] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s0 b7[][1][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s0 b8[1] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s0 b9[] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */

struct s1 c0[0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s1 c1[0][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s1 c2[0][1] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s1 c3[1][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s1 c4[][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s1 c5[][0][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s1 c6[][0][1] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s1 c7[][1][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */

struct s2 d0[0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s2 d1[0][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s2 d2[0][1] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s2 d3[1][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s2 d4[][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s2 d5[][0][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s2 d6[][0][1] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
struct s2 d7[][1][0] = { 1, 2 }; /* { dg-warning "excess elements|near init" } */
