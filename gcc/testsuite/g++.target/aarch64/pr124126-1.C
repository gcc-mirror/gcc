/* { dg-do compile } */
/* { dg-additional-options "-g" } */
/* PR target/124126 */
/* Make sure an array of uint64_t[8] works when
   used before the inlcude of arm_acle.h.  */

typedef unsigned long uint64_t;
void executeSuperscalar(uint64_t (*r)[8]);

#include "arm_acle.h"

void initDatasetItem() {
  uint64_t rl[8];
  executeSuperscalar(&rl);
}
