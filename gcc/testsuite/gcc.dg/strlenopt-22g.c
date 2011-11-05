/* This test needs runtime that provides stpcpy function.  */
/* { dg-do run { target *-*-linux* } } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#define USE_GNU
#include "strlenopt-22.c"

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 1 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
