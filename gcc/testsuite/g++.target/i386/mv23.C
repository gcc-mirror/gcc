/* Test case to check if Multiversioning works.  */
/* { dg-do run } */
/* { dg-require-ifunc "" }  */
/* { dg-require-effective-target static } */
/* { dg-options "-O2 -static -march=x86-64" } */

#include "mv15.C"
