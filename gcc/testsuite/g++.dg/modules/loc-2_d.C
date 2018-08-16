// { dg-additional-options -fmodules-atom }
import stuart;

void foo ()
{
  frob (2); /* { dg-error "invalid conversion" } */
}

// { dg-regexp "In file of module bob,\n +imported at \[^\n]*loc-2_b.C:5,\n +of module stuart,\n +imported at \[^\n]*loc-2_d.C:2:\n\[^\n]*loc-2_a.C:5:18: note:.*" }
