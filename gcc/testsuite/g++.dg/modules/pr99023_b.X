// PR c++/99023, ICE
// { dg-require-effective-target c++20_only }
// { dg-additional-options {-x c++-system-header iostream -fmodules-ts -flang-info-include-translate=<initializer_list> --param ggc-min-expand=0} }

// { dg-prune-output {linker input file unused} }

// { dg-regexp {[^\n]*: note: include '[^\n]*[/\\]initializer_list' translated to import\n} }
NO DO NOT COMPILE
