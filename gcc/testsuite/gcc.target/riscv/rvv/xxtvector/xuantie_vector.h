/* Wrapper of xuantie_vector.h, prevent xuantie_vector.h including stdint.h from
   C library, that might cause problem on testing RV32 related testcase when
   we disable multilib.  */
#ifndef _XUANTIE_VECTOR_WRAP_H

#define _GCC_WRAP_STDINT_H
#include "stdint-gcc.h"
#include_next <xuantie_vector.h>
#define _XUANTIE_VECTOR_WRAP_H

#endif
