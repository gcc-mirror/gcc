#include <stdint.h>

/* Dummy declarations for common TM supporting functions.  */

/* These must be in sync with both libitm/libitm.h and the TM builtin
   definitions in gcc/gtm-builtins.def.  */

#define noinline __attribute__((noinline,noclone,used))

#ifdef __i386__
/* Only for 32-bit x86.  */
# define ITM_REGPARM	__attribute__((regparm(2)))
#else
# define ITM_REGPARM
#endif

ITM_REGPARM noinline uint32_t _ITM_beginTransaction(uint32_t a, ...) { asm(""); }
ITM_REGPARM noinline void _ITM_commitTransaction (void) { asm(""); }
ITM_REGPARM noinline void _ITM_WU4 (void *a, uint32_t b) { asm(""); }
ITM_REGPARM noinline void _ITM_WU8 (void *a, uint64_t b) { asm(""); }
noinline void _ITM_registerTMCloneTable (void) { asm(""); }
noinline void _ITM_deregisterTMCloneTable (void) { asm(""); }
