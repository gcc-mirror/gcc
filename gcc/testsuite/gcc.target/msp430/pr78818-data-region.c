/* { dg-do compile } */
/* { dg-options "-mdata-region=either" } */

/* { dg-final { scan-assembler-not "\\.either" } } */

#include "pr78818-real.c"
