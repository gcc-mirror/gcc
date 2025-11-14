// { dg-additional-options "-fmodules" }
module;

#include <string.h>

export module M;

namespace N {
  export using ::memset;
}
