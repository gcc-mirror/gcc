// { dg-additional-options -fmodules-atom }
import stuart;
import bob;
import stuart;

void foo ()
{
  frob (2); // { dg-error "invalid conversion" }
}

// { dg-regexp "In file of module bob,\n +imported at \[^\n]*loc-2_e.C:3:\n\[^\n]*loc-2_a.C:5:18: note:.*" }
