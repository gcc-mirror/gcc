// { dg-additional-options "-fmodules" }

import M;
struct S::X {};  // { dg-error "in global module conflicts with import" }
struct S::X {};  // { dg-error "in global module conflicts with import" }
// { dg-regexp {[^\n]*class-13_a.C:6:10: note: import declared attached to module 'M'\n} }
