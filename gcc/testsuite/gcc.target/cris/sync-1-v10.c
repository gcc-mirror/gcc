/* Check that we can assemble both base atomic variants.  */
/* { dg-do assemble } */
/* { dg-options "-O2 -march=v10" { target { ! march_option } } } */
#include "sync-1.c"
