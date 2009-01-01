/* PR c/36489 */
/* { dg-do compile } */
/* { dg-options "-Woverride-init" } */

struct A { int a; int b[3]; };
union B { int a; int b[3]; };
int t1[10][10]
  = { [1][2] = 11, [1][3] = 12 };
int t2[10][10]
  = { [1][2] = 11, [1] = { [3] = 12 } };	/* { dg-warning "initializ" } */
int t3[10][10]
  = { [1][2] = 11, [1][2] = 12 };		/* { dg-warning "initializ" } */
struct A t4[2]
  = { [0].b[0] = 1, [0].b[1] = 2, [0].b[2] = 3 };
struct A t5[2]
  = { [0].b[0] = 1, [0].b[1] = 2, [0].b = { 3 } }; /* { dg-warning "initializ" } */
union B t6
  = { .b[0] = 1, .b[1] = 2, .b[2] = 3 };
union B t7
  = { .b[0] = 1, .b[1] = 2, .b = { 2 } };	/* { dg-warning "initializ" } */
union B t8
  = { .b[0] = 1, .b[1] = 2, .b[1] = 3 };	/* { dg-warning "initializ" } */
