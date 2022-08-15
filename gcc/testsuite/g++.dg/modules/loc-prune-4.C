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


// { dg-final { scan-lang-dump {Ordinary maps:2 locs:12288 range_bits:5} module } }
// { dg-final { scan-lang-dump { 1 source file names\n Source file...=[^\n]*loc-prune-4.C\n} module } }
// { dg-final { scan-lang-dump { Span:0 ordinary \[[0-9]+\+12288,\+4096\)->\[0,\+4096\)} module } }
// { dg-final { scan-lang-dump { Span:1 ordinary \[[0-9]+\+40960,\+8192\)->\[4096,\+8192\)} module } }
