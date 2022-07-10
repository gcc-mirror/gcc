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

// { dg-final { scan-lang-dump { Macro maps:0 locs:0} module } }
// { dg-final { scan-lang-dump-not { Macro:. _GLIBCXX_VISIBILITY} module } }
