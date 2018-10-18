// { dg-additional-options "-fmodules-ts -fforce-module-macros" }

import "macro-4_b.H";
import "macro-4_a.H";

// { dg-regexp "\[^\n]*macro-4_d.C: error: inconsistent imported macro definition 'TWO'\nIn module \"macro-4_b.H\", imported at \[^\n]*macro-4_d.C:\[0-9]*:\n\[^\n]*macro-4_b.H:\[0-9]*: note: #define TWO 2a\nIn module \"macro-4_a.H\", imported at \[^\n]*macro-4_d.C:\[0-9]*:\n\[^\n]*macro-4_a.H:\[0-9]*: note: #define TWO 2\n" }

// { dg-regexp "\[^\n]*macro-4_d.C: error: inconsistent imported macro definition 'THREE'\nIn module \"macro-4_b.H\", imported at \[^\n]*macro-4_d.C:\[0-9]*:\n\[^\n]*macro-4_b.H:\[0-9]*: note: #define THREE 3b\nIn module \"macro-4_a.H\", imported at \[^\n]*macro-4_d.C:\[0-9]*:\n\[^\n]*macro-4_a.H:\[0-9]*: note: #define THREE 3\n" }
