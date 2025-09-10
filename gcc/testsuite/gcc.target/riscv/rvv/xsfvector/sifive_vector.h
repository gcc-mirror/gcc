/* Wrapper of sifive_vector.h, prevent sifive_vector.h including stdint.h from
   C library, that might cause problem on testing RV32 related testcase when
   we disable multilib.  */
#ifndef _SIFIVE_VECTOR_WRAP_H

#define _GCC_WRAP_STDINT_H
#include "stdint-gcc.h"
#include_next <riscv_vector.h>
#define _SIFIVE_VECTOR_WRAP_H

#endif
