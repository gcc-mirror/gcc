/* { dg-do compile } */
/* { dg-options "-O3 -fPIC" } */

#include "tls.h"

/* foo must use the global dynamic model.
   __tls_get_offset must be referenced through PLT.  */

/* { dg-final { scan-assembler-times {\tbrasl\t%r14,__tls_get_offset@PLT:tls_gdcall:foo\n} 1 } } */

/* foostatic must use the local dynamic model.
   __tls_get_offset must be referenced through PLT.  */

/* { dg-final { scan-assembler-times {\tbrasl\t%r14,__tls_get_offset@PLT:tls_ldcall} 1 } } */
