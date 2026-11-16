/* PR target/128039: ICE when including arm_mve.h after a PCH.  */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

#include "pr128039.h"
#include <arm_mve.h>

int func(int) { return 0; }
