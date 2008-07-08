/* { dg-options "-w" } */

#ifndef SKIP_ATTRIBUTE

#include "compat-common.h"
#include "vector-defs.h"
#include "vector-check.h"

TEST (8, qi, 101)
TEST (16, qi, 101)
TEST (32, qi, 90)
TEST (2, hi, 201)
TEST (4, hi, 202)
TEST (8, hi, 203)
TEST (16, hi, 203)
TEST (2, si, 301)
TEST (4, si, 302)
TEST (8, si, 303)
TEST (1, di, 401)
TEST (2, di, 402)
TEST (4, di, 403)

#endif
