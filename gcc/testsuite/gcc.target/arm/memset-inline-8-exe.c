/* { dg-do run } */
/* { dg-skip-if "Don't inline memset using neon instructions" { ! arm_tune_string_ops_prefer_neon } } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-save-temps -Os -fno-inline"  } */
/* { dg-add-options "arm_neon" } */

#include "./memset-inline-8.c"
