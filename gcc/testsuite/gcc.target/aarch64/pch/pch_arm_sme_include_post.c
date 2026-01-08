#include "pch_arm_sme_include_post.h"
#include <arm_sme.h>

void st(svbool_t pg, void *ptr, uint32_t slice_base) 
  __arm_streaming __arm_inout("za"){
  svst1_hor_za8(0, slice_base, pg, ptr);
}
