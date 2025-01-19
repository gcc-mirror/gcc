/* { dg-do run } */
/* { dg-skip-if "Don't inline memset using neon instructions" { ! arm_tune_string_ops_prefer_neon } } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-save-temps -O2 -fno-inline" } */
/* { dg-add-options "arm_neon" } */

#include "./memset-inline-5.c"
