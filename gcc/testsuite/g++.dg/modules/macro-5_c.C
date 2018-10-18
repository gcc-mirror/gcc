// { dg-additional-options "-fforce-module-macros" }

import "macro-5_a.H";
import "macro-5_b.H";


// { dg-regexp "\[^\n]*macro-5_c.C: error: inconsistent imported macro definition 'baz'\nIn module imported at \[^\n]*macro-5_c.C:3:1:\n\"macro-5_a.H\": note: #define baz 1\nIn module imported at \[^\n]*macro-5_c.C:4:1:\n\"macro-5_b.H\": note: #define baz 2\n" }
