// { dg-additional-options {-Wno-pedantic -fpreprocessed -fdirectives-only -fdump-lang-module-lineno-vops -fmodules-ts} }
module ;

# 4 "inc_b" 1
#define _GLIBCXX_VISIBILITY(V) __attribute__ ((__visibility__ (#V)))
#define _GLIBCXX_BEGIN_NAMESPACE_VERSION 
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
}
# 11 "" 2

export  module  hello;
export  import  :format;
// { dg-module-cmi hello }

// { dg-final { scan-lang-dump { Macro maps:0 locs:0} module } }
// { dg-final { scan-lang-dump-not { Macro:. _GLIBCXX_VISIBILITY} module } }
