// { dg-additional-options "-fmodules-ts" }

import stuart;
import bob;
import stuart;

void foo ()
{
  frob (2); // { dg-error "invalid conversion" }
}

// { dg-regexp "In module bob, imported at \[^\n]*loc-2_e.C:4:\n\[^\n]*loc-2_a.C:6:18: note:.*" }
