/* PR target/114696 */
/* { dg-do assemble { target { apxf && { { ! ia32 } && { ! *-*-darwin* } } } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-mapxf -mtune-ctrl=enable_ndd_mem -O2 -mx32 -maddress-mode=long" } */

#include "apx-ndd-x32-2a.c"
