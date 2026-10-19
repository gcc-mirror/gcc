/* PR rtl-optimization/127483 */
/* { dg-do compile } */
/* { dg-require-effective-target aarch64_mbig_endian } */
/* Avoid a dependence on big-endian C library headers.  */
/* { dg-options "-O2 -mbig-endian -ffreestanding" } */

#include "pr127483.c"
