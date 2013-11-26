/* This test needs runtime that provides stpcpy and __*_chk functions.  */
/* { dg-do run { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#define USE_GNU
#define FORTIFY_SOURCE 2
#include "strlenopt-4.c"

/* { dg-final { scan-tree-dump-times "strlen \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "__memcpy_chk \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "__strcpy_chk \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "__strcat_chk \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "__stpcpy_chk \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 4 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 5 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
