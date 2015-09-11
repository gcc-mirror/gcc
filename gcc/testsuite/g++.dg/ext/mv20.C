/* Test case to check if Multiversioning works.  */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-ifunc "" }  */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fPIE -pie -march=x86-64" } */

#include "mv15.C"
