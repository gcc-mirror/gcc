// { dg-additional-options "-fmodules-ts" }

import kevin;

void foo ()
{
  frob (2); // { dg-error "invalid conversion" }
}

// { dg-regexp "In module bob, imported at \[^\n]*loc-2_c.C:7,\nof module kevin, imported at \[^\n]*loc-2_f.C:3:\n\[^\n]*loc-2_a.C:6:18: note:.*" }
