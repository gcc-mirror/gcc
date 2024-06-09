/* { dg-do run } */
/* { dg-options "-finline-stringops=memmove -save-temps -g0 -fno-lto" } */

#include "../../gcc.c-torture/execute/builtins/memmove.c"

#include "../../gcc.c-torture/execute/builtins/lib/main.c"

/* { dg-final { scan-assembler-not {\mmemmove\M} } } */
