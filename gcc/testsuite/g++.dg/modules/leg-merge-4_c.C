// { dg-additional-options -fmodules-ts }

import "leg-merge-4_a.H";
import "leg-merge-4_b.H";

void foo ()
{
  2[0]; // { dg-error "" }
  bob = 5;
  frob ();
  X *p;
}

// { dg-regexp "\nIn module \[^\n]*leg-merge-4_b.H, imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_b.H:4:\[0-9]*: error: conflicting global module declaration 'float bob'\nIn module \[^\n]*leg-merge-4_a.H, imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_a.H:4:\[0-9]*: note: existing declaration 'int bob'\n\[^\n]*leg-merge-4_c.C:9:\[0-9]*: note: during load of binding '::bob'$" }

// { dg-regexp "\nIn module \[^\n]*leg-merge-4_b.H, imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_b.H:5:\[0-9]*: error: conflicting global module declaration 'int frob\\(\\)'\nIn module \[^\n]*leg-merge-4_a.H, imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_a.H:5:\[0-9]*: note: existing declaration 'void frob\\(\\)'\n\[^\n]*leg-merge-4_c.C:10:\[0-9]*: note: during load of binding '::frob'$" }

// { dg-regexp "In module \[^\n]*leg-merge-4_b.H, imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_b.H:6:\[0-9]*: error: conflicting global module declaration 'union X'\nIn module \[^\n]*leg-merge-4_a.H, imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_a.H:6:\[0-9]*: note: existing declaration 'class X'\n\[^\n]*leg-merge-4_c.C:11:\[0-9]*: note: during load of binding '::X'$" }
