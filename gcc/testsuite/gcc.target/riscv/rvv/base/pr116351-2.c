/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f -mabi=lp64d -O3 -ftree-vectorize" } */

#include "pr116351.h"
