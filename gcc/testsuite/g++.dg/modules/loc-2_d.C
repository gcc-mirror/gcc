// { dg-additional-options "-fmodules-ts" }

import stuart;

void foo ()
{
  frob (2); /* { dg-error "invalid conversion" } */
}

// { dg-regexp "In module bob, imported at \[^\n]*loc-2_b.C:6,\nof module stuart, imported at \[^\n]*loc-2_d.C:3:\n\[^\n]*loc-2_a.C:6:18: note:.*" }
