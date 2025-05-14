/* PR preprocessor/116047 */
/* { dg-do preprocess } */
/* { dg-options "-nostdinc -std=c23 -fplugin-arg-location_overflow_plugin-value=0x4ffe0180" } */
#include "location-overflow-test-pr116047-1.h"
/* { dg-final { scan-file location-overflow-test-pr116047.i "static_assert\[^\n\r]\*6\[^\n\r]\*== 6" } } */
