// PR c++/99023, ICE
// { dg-require-effective-target c++20_only }
// { dg-additional-options {-x c++-system-header initializer_list -fmodules-ts --param ggc-min-expand=0} }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

// { dg-prune-output {linker input file unused} }

NO DO NOT COMPILE
