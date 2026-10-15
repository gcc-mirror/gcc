/* PR middle-end/126236 */
/* { dg-do assemble } */
/* { dg-options "-O2 -fduplicate-switch-dispatch -mcmodel=large -fno-pie -save-temps" } */

#include "switch-dispatch-1.c"

/* { dg-final { scan-assembler-times "\\tbr\\tx" 13 } } */
/* { dg-final { scan-assembler-times "\\.Lrtx\[0-9\]+:" 1 } } */
/* { dg-final { scan-assembler-times "adrp\\tx\[0-9\]+, \\.Lrtx" 13 } } */
/* { dg-final { scan-assembler-times "add\\tx\[0-9\]+, x\[0-9\]+, :lo12:\\.Lrtx" 13 } } */
