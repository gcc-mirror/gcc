// { dg-additional-options "-fmodules-ts" }

#define BINKY(X) X

import "macro-2_a.H";
import "macro-2_b.H";

int FOO_OK = BAR_OK(1);

int BAR_BAD;
// { dg-regexp {[^\n]*macro-2_d.C:10:5: error: inconsistent imported macro definition 'BAR_BAD'\nIn module [^\n]*macro-2_a.H, imported at [^\n]*macro-2_d.C:5:\n[^\n]*macro-2_a.H:11:9: note: '#define BAR_BAD\(BAZ\) BINKY\(2\)'\nIn module [^\n]*macro-2_b.H, imported at [^\n]*macro-2_d.C:6:\n[^\n]*macro-2_b.H:21:9: note: '#define BAR_BAD\(BAZ\) BINKY\(3\)'\n} }

int FOO_BAD;
// { dg-regexp {[^\n]*macro-2_d.C:13:5: error: inconsistent imported macro definition 'FOO_BAD'\nIn module [^\n]*macro-2_a.H, imported at [^\n]*macro-2_d.C:5:\n[^\n]*macro-2_a.H:10:9: note: '#define FOO_BAD foo'\nIn module [^\n]*macro-2_b.H, imported at [^\n]*macro-2_d.C:6:\n[^\n]*macro-2_b.H:20:9: note: '#define FOO_BAD foot'\n} }
