/* { dg-do run { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#define USE_GNU
#include "strlenopt-31.c"

/* { dg-final { scan-tree-dump-times "stpcpy \\(" 1 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 2 "strlen1" } } */
/* { dg-final { scan-tree-dump-not "strlen \\(" "strlen1" } } */
