// { dg-additional-options -fmodules-atom }
import kevin;

void foo ()
{
  frob (2); // { dg-error "invalid conversion" }
}

// { dg-regexp "In file of module bob,\n +imported at \[^\n]*loc-2_c.C:6,\n +of module kevin,\n +imported at \[^\n]*loc-2_f.C:2:\n\[^\n]*loc-2_a.C:5:18: note:.*" }
