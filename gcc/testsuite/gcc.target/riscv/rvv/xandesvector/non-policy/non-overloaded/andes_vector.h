/* Wrapper of andes_vector.h, prevent andes_vector.h including stdint.h from
   C library, that might cause problem on testing RV32 related testcase when
   we disable multilib.  */
#ifndef _ANDES_VECTOR_WRAP_H

#define _GCC_WRAP_STDINT_H
#include "stdint-gcc.h"
#include_next <andes_vector.h>
#define _ANDES_VECTOR_WRAP_H

#endif
