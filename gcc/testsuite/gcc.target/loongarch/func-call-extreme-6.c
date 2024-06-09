/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O0 -fno-pic -fno-plt -mexplicit-relocs=none -mcmodel=extreme" } */
/* { dg-final { scan-assembler "test:.*la.global\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,g" } } */
/* { dg-final { scan-assembler "test1:.*la.local\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,f" } } */
/* { dg-final { scan-assembler "test2:.*la.local\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,l" } } */

#include "func-call-extreme-1.c"
