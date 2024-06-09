/* { dg-do compile } */
/* { dg-options "-march=loongarch64 -mabi=lp64d -O2 -mcmodel=extreme -mtls-dialect=trad -fno-plt -mexplicit-relocs=auto -fdump-rtl-final" } */

#include "cmodel-extreme-1.c"

/* a, b, d, e, f, and __tls_get_addr.  */
/* { dg-final { scan-rtl-dump-times "la_pcrel64_two_parts" 6 "final" } } */
