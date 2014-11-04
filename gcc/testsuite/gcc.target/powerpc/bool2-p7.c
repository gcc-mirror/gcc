/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7" } */
/* { dg-final { scan-assembler-not "\[ \t\]and "     } } */
/* { dg-final { scan-assembler-not "\[ \t\]or "      } } */
/* { dg-final { scan-assembler-not "\[ \t\]xor "     } } */
/* { dg-final { scan-assembler-not "\[ \t\]nor "     } } */
/* { dg-final { scan-assembler-not "\[ \t\]eqv "     } } */
/* { dg-final { scan-assembler-not "\[ \t\]andc "    } } */
/* { dg-final { scan-assembler-not "\[ \t\]orc "     } } */
/* { dg-final { scan-assembler-not "\[ \t\]nand "    } } */
/* { dg-final { scan-assembler-not "\[ \t\]vand "    } } */
/* { dg-final { scan-assembler-not "\[ \t\]vor "     } } */
/* { dg-final { scan-assembler-not "\[ \t\]vxor "    } } */
/* { dg-final { scan-assembler-not "\[ \t\]vnor "    } } */
/* { dg-final { scan-assembler     "\[ \t\]xxland "  } } */
/* { dg-final { scan-assembler     "\[ \t\]xxlor "   } } */
/* { dg-final { scan-assembler     "\[ \t\]xxlxor "  } } */
/* { dg-final { scan-assembler     "\[ \t\]xxlnor "  } } */
/* { dg-final { scan-assembler     "\[ \t\]xxlandc " } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxleqv "  } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxlorc "  } } */
/* { dg-final { scan-assembler-not "\[ \t\]xxlnand " } } */

#ifndef TYPE
typedef int v4si __attribute__ ((vector_size (16)));
#define TYPE v4si
#endif

#include "bool2.h"
