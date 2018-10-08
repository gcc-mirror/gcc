// Test case to check if multiversioning functions that are extern "C"
// generates errors.

// { dg-do compile }

extern "C"
__attribute__ ((target ("default")))
int foo ()  // { dg-message "previously defined here" }
{
  return 0;
}

extern "C"
__attribute__ ((target ("sse4.2")))
int foo () // { dg-error "redefinition" }
{
  return 1;
}
