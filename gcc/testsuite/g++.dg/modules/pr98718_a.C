// PR 98718 ICE with macro location data
// { dg-additional-options {-Wno-pedantic -fpreprocessed -fdirectives-only -fdump-lang-module-lineno -fmodules-ts} }
module ;

# 4 "inc_a" 1
#define _GLIBCXX_VISIBILITY(V) __attribute__ ((__visibility__ (#V)))

namespace std _GLIBCXX_VISIBILITY(default)
{

}
# 11 "" 2

export  module  hello:format;
// { dg-module-cmi hello:format }

// { dg-final { scan-lang-dump { Ordinary:4 maps hwm:[0-9]* macro:1 maps lwm:214[0-9]*} module } }
// { dg-final { scan-lang-dump { Span:2 macro:0 _GLIBCXX_VISIBILITY 10/11\*2 locations } module } }
