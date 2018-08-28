// { dg-additional-options -fmodules-atom }

import "macro-2_a.H";
import "macro-2_b.H";

// { dg-regexp "In file of module \"macro-2_b.H\",\n *imported at \[^\n]*macro-2_d.C:4:\n\[^\n]*macro-2_b.H:21: error: incompatible redefinition of macro 'BAR_BAD'\nIn file of module \"macro-2_a.H\",\n *imported at \[^\n]*macro-2_d.C:3:\n\[^\n]*macro-2_a.H:11: note: current definition\n" }

// { dg-regexp "In file of module \"macro-2_b.H\",\n *imported at \[^\n]*macro-2_d.C:4:\n\[^\n]*macro-2_b.H:20: error: incompatible redefinition of macro 'FOO_BAD'\nIn file of module \"macro-2_a.H\",\n *imported at \[^\n]*macro-2_d.C:3:\n\[^\n]*macro-2_a.H:10: note: current definition\n" }

