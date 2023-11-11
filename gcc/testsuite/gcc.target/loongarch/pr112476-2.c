/* PR target/112476: ICE with -mlasx */
/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mfpu=64 -mabi=lp64d -mlasx" } */

#include "pr112476-1.c"
