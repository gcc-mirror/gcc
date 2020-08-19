// { dg-additional-options {-fmodules-ts} }

enum ONE {Q};
enum {TWO, DREI};
enum FOUR {B = 3};
enum FIVE {C, D};

import "enum-bad-1_a.H";






ONE one;
// { dg-regexp {In module [^\n]*enum-bad-1_a.H, imported at [^\n]*enum-bad-1_b.C:8:\n[^\n]*enum-bad-1_a.H:5:6: error: definition of 'enum ONE' does not match\n[^\n]*enum-bad-1_b.C:3:6: note: existing definition 'enum ONE'\nIn module [^\n]*enum-bad-1_a.H, imported at [^\n]*enum-bad-1_b.C:8:\n[^\n]*enum-bad-1_a.H:5:11: note: ... this enumerator 'A'\n[^\n]*enum-bad-1_b.C:3:11: note: enumerator 'Q' does not match ...\n[^\n]*enum-bad-1_b.C:15:1: note: during load of binding '::ONE'\n} }

int i = TWO;
// { dg-regexp {In module [^\n]*enum-bad-1_a.H, imported at [^\n]*enum-bad-1_b.C:8:\n[^\n]*enum-bad-1_a.H:6:6: error: definition of 'enum<unnamed>' does not match\n[^\n]*enum-bad-1_b.C:4:6: note: existing definition 'enum<unnamed>'\nIn module [^\n]*enum-bad-1_a.H, imported at [^\n]*enum-bad-1_b.C:8:\n[^\n]*enum-bad-1_a.H:6:12: note: ... this enumerator 'THREE'\n[^\n]*enum-bad-1_b.C:4:12: note: enumerator 'DREI' does not match ...\n[^\n]*enum-bad-1_b.C:18:9: note: during load of binding '::TWO'\n} }

FOUR four;
// { dg-regexp {In module [^\n]*enum-bad-1_a.H, imported at [^\n]*enum-bad-1_b.C:8:\n[^\n]*enum-bad-1_a.H:7:6: error: definition of 'enum FOUR' does not match\n[^\n]*enum-bad-1_b.C:5:6: note: existing definition 'enum FOUR'\nIn module [^\n]*enum-bad-1_a.H, imported at [^\n]*enum-bad-1_b.C:8:\n[^\n]*enum-bad-1_a.H:7:12: note: ... this enumerator 'B'\n[^\n]*enum-bad-1_b.C:5:12: note: enumerator 'B' does not match ...\n[^\n]*enum-bad-1_b.C:21:1: note: during load of binding '::FOUR'\n} }

FIVE five;
// { dg-regexp {In module [^\n]*enum-bad-1_a.H, imported at [^\n]*enum-bad-1_b.C:8:\n[^\n]*enum-bad-1_a.H:8:6: error: definition of 'enum FIVE' does not match\n[^\n]*enum-bad-1_b.C:6:6: note: existing definition 'enum FIVE'\nIn module [^\n]*enum-bad-1_a.H, imported at [^\n]*enum-bad-1_b.C:8:\n[^\n]*enum-bad-1_a.H:8:18: note: additional enumerators beginning with 'E'\n[^\n]*enum-bad-1_b.C:24:1: note: during load of binding '::FIVE'\n} }

