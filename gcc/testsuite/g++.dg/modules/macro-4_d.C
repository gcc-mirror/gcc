// { dg-additional-options "-fmodules-ts -Winvalid-imported-macros" }

import "macro-4_b.H";
import "macro-4_a.H";

// { dg-regexp {[^\n]*macro-4_d.C: warning: inconsistent imported macro definition 'TWO' \[-Winvalid-imported-macros\]\nIn module [^\n]*macro-4_b.H, imported at [^\n]*macro-4_d.C:[0-9]*:\n[^\n]*macro-4_b.H:[0-9]*: note: .#define TWO 2a.\nIn module [^\n]*macro-4_a.H, imported at [^\n]*macro-4_d.C:[0-9]*:\n[^\n]*macro-4_a.H:[0-9]*: note: .#define TWO 2.\n} }

// { dg-regexp {[^\n]*macro-4_d.C: warning: inconsistent imported macro definition 'THREE' \[-Winvalid-imported-macros\]\nIn module [^\n]*macro-4_b.H, imported at [^\n]*macro-4_d.C:[0-9]*:\n[^\n]*macro-4_b.H:[0-9]*: note: .#define THREE 3b.\nIn module [^\n]*macro-4_a.H, imported at [^\n]*macro-4_d.C:[0-9]*:\n[^\n]*macro-4_a.H:[0-9]*: note: .#define THREE 3.\n} }
