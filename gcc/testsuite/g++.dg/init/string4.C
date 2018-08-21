// PR tree-optimization/71625 - missing strlen optimization on different
// array initialization style

// Verify that zero-length array initialization results in the expected
// array sizes and in the expected diagnostics.  See init-string-3.c
// for the corresponding C test.

// { dg-do compile }
// { dg-options "-Wall -Wno-unused-local-typedefs -fpermissive" }

#define A(expr) typedef char A[-1 + 2 * !!(expr)];

const char a[] = { };

A (sizeof a == 0);


const char b[0] = { };

A (sizeof b == 0);

// Also verify that the error is "too many initializers for
// 'const char [0]'" and not "initializer-string is too long."
const char c[0] = { 1 };      // { dg-error "too many initializers for .const char \\\[0]" }

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

  const char b[0] = { 0 };    // { dg-error "too many initializers" }

  A (sizeof b == 0);

  const char c[0] = "";       // { dg-warning "too long" }

  A (sizeof c == 0);
}


void test_compound_zero_length (void)
{
  A (sizeof (const char[]){ } == 0);
  A (sizeof (const char[0]){ } == 0);
  A (sizeof (const char[0]){ 0 } == 0);    // { dg-error "too many" }
  A (sizeof (const char[0]){ 1 } == 0);    // { dg-error "too many" }
  A (sizeof (const char[0]){ "" } == 0);   // { dg-warning "too long" }
  A (sizeof (const char[0]){ "1" } == 0);  // { dg-warning "too long" }
}
