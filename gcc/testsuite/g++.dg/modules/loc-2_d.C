
import stuart;

void foo ()
{
  frob (2); /* { dg-error "invalid conversion" } */
}

// { dg-regexp "In module bob, imported at \[^\n]*loc-2_b.C:5,\nof module stuart, imported at \[^\n]*loc-2_d.C:2:\n\[^\n]*loc-2_a.C:5:18: note:.*" }
