/* Check that we can assemble both base atomic variants.  */
/* { dg-do assemble } */
/* { dg-options "-O2 -march=v10" { target { ! march_option } } } */
/* { dg-additional-options "-mno-unaligned-atomic-may-use-library" { target cris*-*-linux* } } */
#include "sync-1.c"
