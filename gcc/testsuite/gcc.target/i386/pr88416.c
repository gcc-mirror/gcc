/* PR rtl-optimization/88416 */
/* { dg-do compile } */
/* { dg-options "-O1 -fvar-tracking-assignments -fno-forward-propagate --param max-cse-insns=1" } */

#include "writeeflags-1.c"
