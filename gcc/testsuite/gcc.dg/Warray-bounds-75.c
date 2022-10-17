/* Sanity test for Warray-bounds-7[1-4].c.  Also verify the expected
   inlining stack.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#include "Warray-bounds-71.h"

// { dg-regexp "In function 'f1'," "In function f1" { target *-*-* } 0 }
// { dg-regexp "inlined from 'f2' at \[^\\n\\r\]+\[\\n\\r\]" "inlined from f2" { target *-*-* } 0 }
// { dg-regexp "inlined from 'f3' at \[^\\n\\r\]+\[\\n\\r\]" "inlined from f3" { target *-*-* } 0 }
// { dg-regexp "inlined from 'f4' at \[^\\n\\r\]+\[\\n\\r\]" "inlined from f4" { target *-*-* } 0 }
// { dg-message "Warray-bounds-71.h:\\d+:\\d+: warning: array subscript 6 is outside array bounds of 'int\\\[4]'" "warning" { target *-*-* } 0 }
