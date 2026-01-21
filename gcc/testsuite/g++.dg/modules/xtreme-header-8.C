// PR c++/115126
// { dg-additional-options "-fmodules-ts -Wtemplate-names-tu-local" }
// { dg-module-cmi xstd }
// { dg-skip-if "required hosted libstdc++ for any in xtreme-header.h" { ! hostedlib } }

export module xstd;
extern "C++" {
  #include "xtreme-header.h"
}
