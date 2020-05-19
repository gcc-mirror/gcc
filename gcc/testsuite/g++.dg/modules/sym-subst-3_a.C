// { dg-additional-options "-fmodules-ts -std=c++17" }
export module t.s;
// { dg-module-cmi t.s }

struct s;

export s *S;

// { dg-final { scan-assembler {S:} { target { ! *-*-darwin* } } } }
// { dg-final { scan-assembler {.zerofill __DATA,__pu_bss.,_S,} { target *-*-darwin* } } }
