/* Test case to check if Multiversioning works.  */
/* { dg-do run } */
/* { dg-require-ifunc "" }  */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fPIE -pie -march=x86-64" } */

#include "mv14.C"
