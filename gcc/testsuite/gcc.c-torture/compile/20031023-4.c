/* On SPARC64/SPARC-V9 it fails because of a back-end problem, except with -m32. */
/* { dg-xfail-if "PR target/6466" { "sparc64-*-*" "sparcv9-*-*" } { "*" } { "-m32" } } */
/* On regular SPARC it doesn't fail, except with -m64. */
/* { dg-xfail-if "PR target/6466" { "sparc-*-*" } { "-m64" } { "" } } */

#define ASIZE 0x80000000UL
#include "20031023-1.c"
