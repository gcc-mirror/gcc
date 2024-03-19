/* { dg-do compile } */
/* { dg-options "-march=armv8-a -O2" } */

#include <arm_acle.h>

/* Check that we can generate the immediate-offset addressing
   mode for PRFM.  */

/* Access kind specifiers.  */
#define PLD 0
#define PST 1
/* Cache levels.  */
#define L1  0
#define L2  1
#define L3  2
#define SLC 3
/* Retention policies.  */
#define KEEP 0
#define STRM 1

void
prefetch_for_read_write (void *a)
{
  __pldx (PLD, L1, KEEP, a);
  __pldx (PLD, L1, STRM, a);
  __pldx (PLD, L2, KEEP, a);
  __pldx (PLD, L2, STRM, a);
  __pldx (PLD, L3, KEEP, a);
  __pldx (PLD, L3, STRM, a);
  __pldx (PLD, SLC, KEEP, a);
  __pldx (PLD, SLC, STRM, a);
  __pldx (PST, L1, KEEP, a);
  __pldx (PST, L1, STRM, a);
  __pldx (PST, L2, KEEP, a);
  __pldx (PST, L2, STRM, a);
  __pldx (PST, L3, KEEP, a);
  __pldx (PST, L3, STRM, a);
  __pldx (PST, SLC, KEEP, a);
  __pldx (PST, SLC, STRM, a);
}

/* { dg-final { scan-assembler "prfm\tPLDL1KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLDL1STRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLDL2KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLDL2STRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLDL3KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLDL3STRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLDSLCKEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLDSLCSTRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPSTL1KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPSTL1STRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPSTL2KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPSTL2STRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPSTL3KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPSTL3STRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPSTSLCKEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPSTSLCSTRM, \\\[x\[0-9\]+\\\]" } } */

void
prefetch_simple (void *a)
{
  __pld (a);
  __pli (a);
}

/* { dg-final { scan-assembler "prfm\tPLDL1KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLIL1KEEP, \\\[x\[0-9\]+\\\]" } } */

void
prefetch_instructions (void *a)
{
  __plix (L1, KEEP, a);
  __plix (L1, STRM, a);
  __plix (L2, KEEP, a);
  __plix (L2, STRM, a);
  __plix (L3, KEEP, a);
  __plix (L3, STRM, a);
  __plix (SLC, KEEP, a);
  __plix (SLC, STRM, a);
}

/* { dg-final { scan-assembler "prfm\tPLIL1KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLIL1STRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLIL2KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLIL2STRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLIL3KEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLIL3STRM, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLISLCKEEP, \\\[x\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "prfm\tPLISLCSTRM, \\\[x\[0-9\]+\\\]" } } */

