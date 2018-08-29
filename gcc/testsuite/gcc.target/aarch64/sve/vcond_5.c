/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-trapping-math" } */

/* The difference here is that nueq can use LTGT.  */

#include "vcond_4.c"

/* See PR 86753 for the reason behind the XFAILs.  */

/* 5 for eqand 5 for ueq.  */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0} 10 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s} 20 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0} 5 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s} 10 { xfail *-*-* } } } */

/* 5 for lt, 5 for ult, 5 for nueq and 5 for nult.  */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0} 20 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s} 40 { xfail *-*-* } } } */

/* 5 for le, 5 for ule and 5 for nule.  */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0} 15 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s} 30 { xfail *-*-* } } } */

/* 5 for gt, 5 for ugt, 5 for nueq and 5 for nugt.  */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0} 20 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s} 40 { xfail *-*-* } } } */

/* 5 for ge, 5 for uge and 5 for nuge.  */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0} 15 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s} 30 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-not {\tfcmuo\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0} } } */
/* 3 loops * 5 invocations for ordered, unordered amd ueq.  */
/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s} 45 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0} 14 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d} 28 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0} 7 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d} 14 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0} 28 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d} 56 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0} 21 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d} 42 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0} 28 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d} 56 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0} 21 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d} 42 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-not {\tfcmuo\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0} } } */
/* 3 loops * 5 invocations, with 2 invocations having ncopies == 2,
   for ordered, unordered and ueq.  */
/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d} 63 { xfail *-*-* } } } */
