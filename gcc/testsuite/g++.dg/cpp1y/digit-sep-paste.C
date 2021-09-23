// Test token pasting with digit separators avoided for preprocessed output.
// { dg-do compile { target c++14 } }
// { dg-options "-save-temps" }

#define ZERO 0

int
f ()
{
  return ZERO'0'0; /* { dg-error "expected" } */
}
