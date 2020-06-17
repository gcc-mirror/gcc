/* PR middle-end/91647 - missing -Warray-bounds accessing a zero-length array
   of a declared object
   { dg-do "compile" }
   { dg-options "-O2 -Wall" } */

struct __attribute__ ((aligned (16))) A16
{
  __INT64_TYPE__ i8;
  __INT16_TYPE__ i2;
  __INT16_TYPE__ a2[];
};

struct A16 a0 = { };

void test_a0 (void)
{
  // The first three elements fit in the tail padding.
  a0.a2[0] = 0; a0.a2[1] = 1; a0.a2[2] = 2;

  a0.a2[3] = 3;     // { dg-warning "array subscript 3 is above array bounds of 'short int\\\[]'" }
}


struct A16 a1 = { .a2 = { 1 } };

void test_a1 (void)
{
  a1.a2[0] = 0; a1.a2[1] = 1; a1.a2[2] = 2;

  a1.a2[3] = 3;     // { dg-warning "array subscript 3 is above array bounds of 'short int\\\[]'" }
}


struct A16 a2 = { .a2 = { 1, 2 } };

void test_a2 (void)
{
  a2.a2[0] = 0; a2.a2[1] = 1; a2.a2[2] = 2;

  a2.a2[3] = 3;     // { dg-warning "array subscript 3 is above array bounds of 'short int\\\[]'" }
}


struct A16 a3 = { .a2 = { 1, 2, 3 } };

void test_a3 (void)
{
  a3.a2[0] = 0; a3.a2[1] = 1; a3.a2[2] = 2;

  a3.a2[3] = 3;     // { dg-warning "array subscript 3 is above array bounds of 'short int\\\[]'" }
}


struct A16 a4 = { .a2 = { 1, 2, 3, 4 } };

void test_a4 (void)
{
  a4.a2[0] = 0; a4.a2[1] = 1; a4.a2[2] = 2; a4.a2[3] = 3;

  a4.a2[4] = 4;     // { dg-warning "array subscript 4 is above array bounds of 'short int\\\[]'" }
}


struct A16 a5 = { .a2 = { 1, 2, 3, 4, 5 } };

void test_a5 (void)
{
  a5.a2[0] = 0; a5.a2[1] = 1; a5.a2[2] = 2; a5.a2[3] = 3; a5.a2[4] = 4;

  a5.a2[5] = 5;     // { dg-warning "array subscript 5 is above array bounds of 'short int\\\[]'" }
}


struct A16 a6 = { .a2 = { 1, 2, 3, 4, 5, 6 } };

void test_a6 (void)
{
  a6.a2[0] = 0; a6.a2[1] = 1; a6.a2[2] = 2; a6.a2[3] = 3; a6.a2[4] = 4;
  a6.a2[5] = 5;

  a6.a2[6] = 6;     // { dg-warning "array subscript 6 is above array bounds of 'short int\\\[]'" }
}


struct A16 a7 = { .a2 = { 1, 2, 3, 4, 5, 6, 7 } };

void test_a7 (void)
{
  a7.a2[0] = 0; a7.a2[1] = 1; a7.a2[2] = 2; a7.a2[3] = 3; a7.a2[4] = 4;
  a7.a2[5] = 5; a7.a2[5] = 5; a7.a2[6] = 6;

  a7.a2[7] = 7;     // { dg-warning "array subscript 7 is above array bounds of 'short int\\\[]'" }
}


struct A16 a8 = { .a2 = { 1, 2, 3, 4, 5, 6, 7, 8 } };

void test_a8 (void)
{
  a8.a2[0] = 0; a8.a2[1] = 1; a8.a2[2] = 2; a8.a2[3] = 3; a8.a2[4] = 4;
  a8.a2[5] = 5; a8.a2[5] = 5; a8.a2[6] = 6; a8.a2[7] = 7;

  a8.a2[8] = 8;     // { dg-warning "array subscript 8 is above array bounds of 'short int\\\[]'" }
}


struct A16 a9 = { .a2 = { 1, 2, 3, 4, 5, 6, 7, 8, 9 } };

void test_a9 (void)
{
  a8.a2[0] = 8; a8.a2[1] = 7; a8.a2[2] = 6; a8.a2[3] = 5; a8.a2[4] = 4;
  a8.a2[5] = 3; a8.a2[5] = 2; a8.a2[6] = 1; a8.a2[7] = 0;

  a8.a2[9] = 8;     // { dg-warning "array subscript 9 is above array bounds of 'short int\\\[]'" }
}
