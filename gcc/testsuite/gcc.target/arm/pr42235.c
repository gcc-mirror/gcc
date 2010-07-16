/* { dg-options "-mthumb -O2 -march=armv5te" }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-final { scan-assembler-not "add\[\\t \]*r.,\[\\t \]*r.,\[\\t \]*\#1" } } */
/* { dg-final { scan-assembler-not "add\[\\t \]*r.,\[\\t \]*\#1" } } */

#include <string.h>

int foo (char *x)
{
  memset (x, 0, 6);
}
