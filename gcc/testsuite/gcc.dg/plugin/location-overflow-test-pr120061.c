/* PR preprocessor/120061 */
/* { dg-do preprocess } */
/* { dg-options "-nostdinc -std=c23 -fplugin-arg-location_overflow_plugin-value=0x61000000" } */
#include "location-overflow-test-pr120061-1.h"
static_assert (__LINE__ == 5, "");
/* { dg-final { scan-file location-overflow-test-pr120061.i "static_assert\[^\n\r]\*5\[^\n\r]\*== 5" } } */
