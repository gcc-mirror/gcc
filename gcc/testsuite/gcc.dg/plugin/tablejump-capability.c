/* PR middle-end/126236 */
/* { dg-do assemble { target aarch64*-*-* } } */
/* { dg-options "-O2 -fchecking -fduplicate-switch-dispatch" } */

void allowed (const unsigned char *, volatile unsigned *)
  __attribute__ ((noipa, target ("arch=armv8-a+crc")));
void rejected (const unsigned char *, volatile unsigned *)
  __attribute__ ((noipa, target ("arch=armv8-a+nocrc")));
void vetoed (const unsigned char *, volatile unsigned *)
  __attribute__ ((noipa, target ("arch=armv8-a+crc")));

#define TEST_NAME allowed
#include "tablejump-test.h"
#undef TEST_NAME
#define TEST_NAME rejected
#include "tablejump-test.h"
#undef TEST_NAME
#define TEST_NAME vetoed
#include "tablejump-test.h"

int
ordinary (int x)
{
  return x + 1;
}
