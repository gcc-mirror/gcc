/* { dg-do assemble { target { rv64 } } } */
/* { dg-options "-O3 -march=rv64imafdc_xtheadfmemidx_xtheadmemidx -mabi=lp64d -save-temps" } */

#include "xtheadmemidx-fpr-int.h"

/* { dg-final { scan-assembler {\mth\.fsrd\s} } } */
/* { dg-final { scan-assembler {\mth\.fsrw\s} } } */
/* { dg-final { scan-assembler-not {\mth\.[ls][a-z]+\s+f[ast][0-9]+,} } } */
