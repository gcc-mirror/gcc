/* PR target/128283 */
/* { dg-do compile } */
/* { dg-options "-O2 -mpconfig -fdump-tree-optimized" } */

#include <immintrin.h>
#include <stdint.h>

struct mktme_kp
{
  uint16_t keyid;
  uint32_t ctrl;
  uint8_t pad[186];
} __attribute__((aligned(256)));

unsigned int
program_mktme (uint16_t kid)
{
  struct mktme_kp kp = { .keyid = kid, .ctrl = 1 };
  size_t d[3] = { (size_t) &kp, 0, 0 };

  return _pconfig_u32 (0 /* MKTME_KEY_PROGRAM */, d);
}

/* { dg-final { scan-tree-dump "kp.ctrl = 1;" "optimized" } } */
