// Test case to check if multiversioning functions that are extern "C"
// generates errors.

// { dg-do compile { target i?86-*-* x86_64-*-* } }

extern "C"
__attribute__ ((target ("default")))
int foo ()  // { dg-error "previously defined here" }
{
  return 0;
}

extern "C"
__attribute__ ((target ("sse4.2")))
int foo () // { dg-error "redefinition" }
{
  return 1;
}
