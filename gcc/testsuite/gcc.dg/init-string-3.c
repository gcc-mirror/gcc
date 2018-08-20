/* PR tree-optimization/71625 - missing strlen optimization on different
   array initialization style

   Verify that zero-length array initialization results in the expected
   array sizes.

   { dg-do compile }
   { dg-options "-Wall -Wno-unused-local-typedefs" }  */

#define A(expr) typedef char A[-1 + 2 * !!(expr)];

const char a[] = { };

A (sizeof a == 0);


const char b[0] = { };

A (sizeof b == 0);


const char c[0] = { 1 };    /* { dg-warning "excess elements" } */

A (sizeof c == 0);


void test_auto_empty (void)
{
  const char a[] = { };

  A (sizeof a == 0);
}

void test_auto_zero_length (void)
{
  const char a[0] = { };

  A (sizeof a == 0);

  const char b[0] = { 0 };    /* { dg-warning "excess elements" } */

  A (sizeof b == 0);

  const char c[0] = "";

  A (sizeof c == 0);
}


void test_compound_zero_length (void)
{
  A (sizeof (const char[]){ } == 0);
  A (sizeof (const char[0]){ } == 0);
  A (sizeof (const char[0]){ 0 } == 0);   /* { dg-warning "excess elements" } */
  A (sizeof (const char[0]){ 1 } == 0);   /* { dg-warning "excess elements" } */
  A (sizeof (const char[0]){ "" } == 0);
  A (sizeof (const char[0]){ "1" } == 0);  /* { dg-warning "too long" } */
}
