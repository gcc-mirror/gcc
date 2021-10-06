/* { dg-do compile } */
/* { dg-options "-O3" } */

#include "tls.h"

/* foo must use the initial-exec model, foostatic must use the local-exec
   model.  */

/* { dg-final { scan-assembler-times {\tear} 4 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\tear} 2 { target { ! lp64 } } } } */
