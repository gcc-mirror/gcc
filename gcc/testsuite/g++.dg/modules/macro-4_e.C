// { dg-additional-options "-fforce-module-macros" }

import "macro-4_b.H";
import "macro-4_a.H";
import "macro-4_c.H";

int a;

#ifndef FIVE
#error bah!
#endif

// { dg-regexp "\[^\n]*macro-4_e.C: error: inconsistent imported macro definition 'TWO'\nIn module \"macro-4_b.H\", imported at \[^\n]*macro-4_e.C:\[0-9]*:\n\[^\n]*macro-4_b.H:\[0-9]*: note: #define TWO 2a\nIn module \"macro-4_a.H\", imported at \[^\n]*macro-4_e.C:\[0-9]*:\n\[^\n]*macro-4_a.H:\[0-9]*: note: #define TWO 2\n" }
