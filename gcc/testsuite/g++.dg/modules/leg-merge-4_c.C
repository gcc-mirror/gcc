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

// { dg-regexp "In module \"leg-merge-4_b.H\", imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_b.H:4:\[0-9]*: error: conflicting global module declaration 'float bob'\nIn module \"leg-merge-4_a.H\", imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_a.H:4:\[0-9]*: note: existing declaration 'int bob'\n\[^\n]*leg-merge-4_c.C:9:\[0-9]*: note: during lazy loading of '::bob@\"leg-merge-4_b.H\"'\n" }

// { dg-regexp "In module \"leg-merge-4_b.H\", imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_b.H:5:\[0-9]*: error: conflicting global module declaration 'int frob\\(\\)'\nIn module \"leg-merge-4_a.H\", imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_a.H:5:\[0-9]*: note: existing declaration 'void frob\\(\\)'\n\[^\n]*leg-merge-4_c.C:10:\[0-9]*: note: during lazy loading of '::frob@\"leg-merge-4_b.H\"'\n" }

// { dg-regexp "In module \"leg-merge-4_b.H\", imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_b.H:6:\[0-9]*: error: conflicting global module declaration 'union X'\nIn module \"leg-merge-4_a.H\", imported at \[^\n]*leg-merge-4_c.C:\[0-9]*:\n\[^\n]*leg-merge-4_a.H:6:\[0-9]*: note: existing declaration 'class X'\n\[^\n]*leg-merge-4_c.C:11:\[0-9]*: note: during lazy loading of '::X@\"leg-merge-4_b.H\"'\n" }
