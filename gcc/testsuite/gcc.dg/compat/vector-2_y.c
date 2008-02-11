/* { dg-options "-w" } */
/* { dg-options "-w -mno-mmx" { target { i?86-*-* x86_64-*-* } } } */

#ifndef SKIP_ATTRIBUTE

#include "compat-common.h"
#include "vector-defs.h"
#include "vector-check.h"

TEST (2, sf, 301.0)
TEST (4, sf, 302.0)
TEST (16, sf, 304.0)
TEST (2, df, 402.0)

#endif
