// { dg-additional-options "-fmodules-ts -Winvalid-imported-macros" }

import "macro-4_b.H";
import "macro-4_a.H";
import "macro-4_c.H";

int stop;

#ifndef FIVE
#error bah!
#endif

// { dg-regexp {[^\n]*macro-4_e.C: warning: inconsistent imported macro definition 'TWO' \[-Winvalid-imported-macros\]\nIn module [^\n]*macro-4_b.H, imported at [^\n]*macro-4_e.C:[0-9]*:\n[^\n]*macro-4_b.H:[0-9]*:[0-9]*: note: .#define TWO 2a.\nIn module [^\n]*macro-4_a.H, imported at [^\n]*macro-4_e.C:[0-9]*:\n[^\n]*macro-4_a.H:[0-9]*:[0-9]*: note: .#define TWO 2.\n} }
