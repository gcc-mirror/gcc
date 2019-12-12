/* PR91639 Line markers for an end-of-file #include   */
/* { dg-do preprocess } */
/* { dg-additional-options -Wno-pedantic } */
/* { dg-additional-files {pr91639-one.h pr91639-two.h} } */

#include "pr91639-one.h"
main

/* { dg-final { scan-file pr91639.i "# 1 \"\[^\n\"\]*pr91639-one.h\" 1\none\n# 1 \"\[^\n\"\]*pr91639-two.h\" 1\ntwo\n# 3 \"\[^\n\"\]*pr91639-one.h\" 2\n# 7 \"\[^\n\"\]*pr91639.c\" 2\nmain\n" } } */
