#include "unpack-vectorize.h"

DEF_ARR (si)
DEF_ARR (ui)
DEF_ARR (sh)
DEF_ARR (uh)
DEF_ARR (sc)
DEF_ARR (uc)

TEST1 (sh, si)
TEST1 (uh, ui)
TEST1 (sc, sh)
TEST1 (uc, uh)

