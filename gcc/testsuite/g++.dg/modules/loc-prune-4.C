//  { dg-additional-options {-Wno-pedantic -fmodules-ts -fdump-lang-module-lineno} }

# 4 "unused" 1
# 5 "" 2

export module foo;

int foo (int) // separate
{


  return 0;
}

int bar (int); // merge lines
int baz (int);


// { dg-final { scan-lang-dump {Ordinary maps:2 locs:49152 range_bits:7} module } }
// { dg-final { scan-lang-dump { 1 source file names\n Source file...=[^\n]*loc-prune-4.C\n} module } }
// { dg-final { scan-lang-dump { Span:0 ordinary \[[0-9]+\+49152,\+16384\)->\[0,\+16384\)} module } }
// { dg-final { scan-lang-dump { Span:1 ordinary \[[0-9]+\+163840,\+32768\)->\[16384,\+32768\)} module } }
