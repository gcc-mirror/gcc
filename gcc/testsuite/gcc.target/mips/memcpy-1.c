/* { dg-options "-fno-common" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\tlbu\t" } } */

#include <string.h>

char c[10];

void
f1 ()
{
  memcpy (c, "123456", 6);
}

void
f2 ()
{
  memcpy (c, &"12345678"[2], 6);
}
