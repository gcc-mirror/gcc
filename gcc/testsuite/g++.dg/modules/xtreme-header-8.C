// PR c++/115126
// { dg-additional-options "-fmodules-ts -Wtemplate-names-tu-local" }
// { dg-module-cmi xstd }

export module xstd;
extern "C++" {
  #include "xtreme-header.h"
}
