/* Test case to check if Multiversioning works.  */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-ifunc "" }  */
/* { dg-require-effective-target static } */
/* { dg-options "-O2 -static -march=x86-64" } */

#include "mv14.C"
