#ifndef GCC_RISCV_VECTOR_BUILTINS_AVAIL_H
#define GCC_RISCV_VECTOR_BUILTINS_AVAIL_H

#include "insn-codes.h"
namespace riscv_vector {

/* Declare an availability predicate for built-in functions.  */
#define AVAIL(NAME, COND)                                                      \
  static unsigned int riscv_vector_avail_##NAME(void) { return (COND); }

} // namespace riscv_vector
#endif
