/* This test needs runtime that provides stpcpy, mempcpy and __*_chk
   functions.  */
/* { dg-do run { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O2 -fdump-tree-strlen" } */
/* Bionic targets don't have mempcpy */
/* { dg-require-effective-target non_bionic } */

#define FORTIFY_SOURCE 2
#include "strlenopt-14g.c"

/* Compared to strlenopt-14gf.c, strcpy_chk with string literal as
   second argument isn't being optimized by builtins.c into
   memcpy.  */
/* { dg-final { scan-tree-dump-times "strlen \\(" 4 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "__memcpy_chk \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "__mempcpy_chk \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "__strcpy_chk \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "__strcat_chk \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "__stpcpy_chk \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 1 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "mempcpy \\(" 2 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 2 "strlen1" } } */
