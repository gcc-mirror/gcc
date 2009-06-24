#undef __section
#define __section(_secname) __attribute__((section(#_secname)))
#undef mep_nop
#define mep_nop() __asm__ volatile ("nop")

#pragma GCC coprocessor available $c0...$c31
#pragma GCC coprocessor call_saved $c6...$c7

#include <intrinsics.h>
