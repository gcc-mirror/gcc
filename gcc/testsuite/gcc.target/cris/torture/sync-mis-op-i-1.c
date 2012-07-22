/* { dg-do run { target *-*-linux* } } */
/* { dg-additional-sources "../sync-1.c" } */
/* { dg-options "-Dop -Dtype=int -mno-unaligned-atomic-may-use-library" } */
#include "sync-mis-op-s-1.c"
