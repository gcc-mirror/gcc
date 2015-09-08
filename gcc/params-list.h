#define DEFPARAM(enumerator, option, nocmsgid, default, min, max) \
  enumerator,
#include "params.def"
#undef DEFPARAM
