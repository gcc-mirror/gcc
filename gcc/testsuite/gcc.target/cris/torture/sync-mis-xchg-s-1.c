/* { dg-do run { target *-*-linux* } } */
/* { dg-additional-sources "../sync-1.c" } */
/* { dg-options "-Dxchg -Dtype=short -mno-unaligned-atomic-may-use-library" } */
#include "sync-mis-op-s-1.c"
