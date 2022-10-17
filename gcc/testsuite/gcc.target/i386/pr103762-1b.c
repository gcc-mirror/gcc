/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -std=gnu11 -fgnu89-inline -fpie" } */
/* { dg-final { scan-assembler-not ".quad\[\\t \]+tunable_list" { target lp64 } } } */
/* { dg-final { scan-assembler-not ".long\[\\t \]+tunable_list" { target { ! lp64 } } } } */

#include "pr103762-1a.c"
