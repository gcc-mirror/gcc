/* PR target/103973 */
/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -save-temps" } */
/* { dg-final { scan-assembler-not "'\t\[v\]?ucomiss" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\t\[v\]?comiss" 4 { target { ! ia32 } } } } */

#define double float
#include "pr103973-13.c"
