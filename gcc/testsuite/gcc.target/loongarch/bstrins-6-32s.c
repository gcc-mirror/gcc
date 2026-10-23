/* { dg-do compile } */
/* { dg-options "-O2 -march=la32v1.0 -mabi=ilp32s" } */
/* { dg-final { scan-assembler "foo:.*\tbstrins\\.w\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,31,1.*foo" } } */
/* { dg-final { scan-assembler "bar:.*\tbstrins\\.w\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,31,15.*bar" } } */

#include "bstrins-6.c"
