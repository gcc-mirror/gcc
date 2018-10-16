
import bob;
import stuart;

void kevin ()
{
  frob (nullptr); // { dg-error "call of overload" }
}

// { dg-regexp "In module stuart, imported at \[^\n]*loc-1_c.C:3:\n\[^\n]*loc-1_b.C:6:12: note:.*" }
// { dg-regexp "In module bob, imported at \[^\n]*loc-1_c.C:2:\n\[^\n]*loc-1_a.C:5:12: note:.*" }


