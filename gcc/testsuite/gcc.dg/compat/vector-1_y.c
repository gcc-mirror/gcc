/* { dg-options "-w" } */
/* { dg-xfail-if "PR target/12916" "sparc*-*-*" "*" "" } */

#include "compat-common.h"
#include "vector-defs.h"
#include "vector-check.h"

TEST (8, qi, 101)
TEST (16, qi, 101)
TEST (2, hi, 201)
TEST (4, hi, 202)
TEST (8, hi, 203)
TEST (2, si, 301)
TEST (4, si, 302)
TEST (1, di, 401)
TEST (2, di, 402)
