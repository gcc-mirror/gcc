# 0 "REALNAME"
# 0 "<built-in>"
# 0 "<command-line>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "REALNAME"

// { dg-additional-options {-fmodule-header -fpreprocessed -fdump-lang-module-lineno} }
// All locations are pruned.
// { dg-final { scan-lang-dump-not {Writing ordinary location maps} module } }
// { dg-final { scan-lang-dump-not {  Span:. ordinary } module } }
