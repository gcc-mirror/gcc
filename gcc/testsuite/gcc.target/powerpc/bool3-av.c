/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O2 -mcpu=power6 -mabi=altivec -maltivec -mno-vsx" } */
/* { dg-final { scan-assembler	   "\[ \t\]and "     } } */
/* { dg-final { scan-assembler	   "\[ \t\]or "      } } */
/* { dg-final { scan-assembler	   "\[ \t\]xor "     } } */
/* { dg-final { scan-assembler	   "\[ \t\]nor "     } } */
/* { dg-final { scan-assembler	   "\[ \t\]andc "    } } */
/* { dg-final { scan-assembler-not "\[ \t\]vand "    } } */
/* { dg-final { scan-assembler-not "\[ \t\]vandc "   } } */
/* { dg-final { scan-assembler-not "\[ \t\]vor "     } } */
/* { dg-final { scan-assembler-not "\[ \t\]vxor "    } } */
/* { dg-final { scan-assembler-not "\[ \t\]vnor "    } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxland "  } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxlor "   } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxlxor "  } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxlnor "  } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxlandc " } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxleqv "  } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxlorc "  } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxlnand " } } */

/* On altivec, for 128-bit types, ORC/ANDC/EQV might not show up, since the
   vector unit doesn't support these, so the appropriate combine patterns may
   not be generated.  */

#ifndef TYPE
#ifdef _ARCH_PPC64
#define TYPE __int128_t
#else
typedef int v4si __attribute__ ((vector_size (16)));
#define TYPE v4si
#endif
#endif

#include "bool3.h"
