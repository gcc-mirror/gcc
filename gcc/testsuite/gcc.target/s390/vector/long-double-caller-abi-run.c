/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include "long-double-callee-abi-scan.c"
#include "long-double-caller-abi-scan.c"
