// { dg-additional-options "-fmodules-ts -std=c++17" }
export module t.s;
// { dg-module-cmi t.s }

struct s;

export s *S;

// { dg-final { scan-assembler {.globa?l[ \t]*_?_ZW1tW1s1S\n} } }
