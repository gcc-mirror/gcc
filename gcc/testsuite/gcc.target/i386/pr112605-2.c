/* PR target/112605 */
/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target split_stack } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fsplit-stack -fpic -mcmodel=large -mforce-indirect-call " } */

#include "pr112605.c"
