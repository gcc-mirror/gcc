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

// { dg-final { scan-lang-dump {Macro:0 _GLIBCXX_VISIBILITY 10/11\*2 locations } module } }
// { dg-final { scan-lang-dump { Ordinary:8 maps hwm:[0-9]* macro:2 maps lwm:214[0-9]*} module } }
// { dg-final { scan-lang-dump { Span:2 macro:0 _GLIBCXX_VISIBILITY 10/11\*2 locations } module } }
// { dg-final { scan-lang-dump { Span:4 macro:1 _GLIBCXX_VISIBILITY 10/11\*2 locations } module } }
