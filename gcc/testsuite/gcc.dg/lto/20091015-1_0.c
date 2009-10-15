/* { dg-lto-do link } */
/* { dg-lto-options {{-fPIC -shared -O2 -flto} {-fPIC -shared -O2 -fwhopr}} } */

#include "20091015-1_b.h"
void diagnostic_initialize (FILE **stream) { *stream = stderr; }
