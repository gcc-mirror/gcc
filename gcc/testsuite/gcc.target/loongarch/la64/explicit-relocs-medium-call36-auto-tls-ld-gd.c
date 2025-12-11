/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mexplicit-relocs=auto -mtls-dialect=trad -mcmodel=medium -fplt" } */
/* { dg-final { scan-assembler "pcaddu18i\t\\\$r1,%call36\\\(__tls_get_addr\\\)" { target { tls_native && loongarch_call36_support } } } } */

#include "./explicit-relocs-auto-tls-ld-gd.c"
