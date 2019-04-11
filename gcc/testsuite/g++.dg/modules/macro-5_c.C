// { dg-additional-options "-fmodules-ts -fforce-module-macros" }

import "macro-5_a.H";
import "macro-5_b.H";


// { dg-regexp {[^\n]*macro-5_c.C: error: inconsistent imported macro definition 'baz'\nIn module [^\n]*macro-5_a.H, imported at [^\n]*macro-5_c.C:3:\n<command-line>: note: #define baz 1\nIn module [^\n]*macro-5_b.H, imported at [^\n]*macro-5_c.C:4:\n<command-line>: note: #define baz 2\n} }
