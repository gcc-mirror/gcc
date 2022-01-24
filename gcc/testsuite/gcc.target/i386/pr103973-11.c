/* PR target/103973 */
/* { dg-do run } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-final { scan-assembler-not "'\tucomiss" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tcomiss" 4 { target { ! ia32 } } } } */

#define double float
#include "pr103973-9.c"
