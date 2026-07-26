// PR c++/115126
// Force the serial PSTL backend.  If oneTBB is installed, <execution>
// pulls third-party headers that expose TU-local entities into the
// module purview, which is ill-formed in a module interface unit.
// { dg-additional-options "-fmodules-ts -Wtemplate-names-tu-local -D_GLIBCXX_USE_TBB_PAR_BACKEND=0" }
// { dg-module-cmi xstd }
// { dg-skip-if "required hosted libstdc++ for any in xtreme-header.h" { ! hostedlib } }

export module xstd;
extern "C++" {
  #include "xtreme-header.h"
}
