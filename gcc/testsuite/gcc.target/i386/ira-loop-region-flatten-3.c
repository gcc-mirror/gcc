/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mabi=sysv -mno-apxf -fdump-rtl-ira" } */

#include "ira-loop-region-flatten-1.c"

/* { dg-final { scan-rtl-dump-not "flatten-cost" "ira" } } */
