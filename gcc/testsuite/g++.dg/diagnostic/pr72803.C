/* Long line, with a close brace at column 511, hence with the insertion
   point for the missing semicolon at column 512.  */
class test {                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  }
# 1 "" 1
// The line directive appears to be necessary to trigger the ICE
// { dg-error "style of line directive is a GCC extension" "" { target *-*-* } .-2 }

/* Verify that we get the best line and column for the diagnostic.
   512 is not representable in the line-maps created for this test.  */
// { dg-error "511: expected .;. after class definition" "" { target *-*-* } 3 }
