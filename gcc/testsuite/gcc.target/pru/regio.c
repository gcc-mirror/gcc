/* __regio_symbol operations. */

/* { dg-do compile } */
/* { dg-options "-Os" } */

#include "regio.h"

void
test_r30_w_const (void)
{
  /* { dg-final { scan-assembler "mov\\tr30, r\[012\]\[0-9\]?" } } */
  __R30 = 1;
}

void
test_r31_w_zext_qi (unsigned char val1)
{
  /* { dg-final { scan-assembler "mov\\tr31, r14.b0" } } */
  __R31 = val1;
}

void
test_r31_w_zext_hi (unsigned short val1)
{
  /* { dg-final { scan-assembler "mov\\tr31, r14.w0" } } */
  __R31 = val1;
}

void
test_r31_w (unsigned int val1)
{
  /* { dg-final { scan-assembler "mov\\tr31, r14" } } */
  __R31 = val1;
}

uint32_t
test_r30_r (void)
{
  /* { dg-final { scan-assembler "mov\\tr14, r30" } } */
  return __R30;
}

void
test_r30_rw (void)
{
  /* { dg-final { scan-assembler "mov\\tr\[012\]\[0-9\]?, r30" } } */
  /* { dg-final { scan-assembler "mov\\tr30, r\[012\]\[0-9\]?" } } */
  __R30 = __R30;
}

void
test_r31_rw (void)
{
  /* { dg-final { scan-assembler "mov\\tr\[012\]\[0-9\]?, r31" } } */
  /* { dg-final { scan-assembler "mov\\tr31, r\[012\]\[0-9\]?" } } */
  __R31 |= 101;
}

