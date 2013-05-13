
/* { dg-do compile } */
/* { dg-options "-O3" } */

#include "vect.x"

/* { dg-final { scan-assembler "orn\\tv" } } */
/* { dg-final { scan-assembler "bic\\tv" } } */
/* { dg-final { scan-assembler "mla\\tv" } } */
/* { dg-final { scan-assembler "mls\\tv" } } */
/* { dg-final { scan-assembler "smax\\tv" } } */
/* { dg-final { scan-assembler "smin\\tv" } } */
/* { dg-final { scan-assembler "umax\\tv" } } */
/* { dg-final { scan-assembler "umin\\tv" } } */
/* { dg-final { scan-assembler "umaxv" } } */
/* { dg-final { scan-assembler "uminv" } } */
/* { dg-final { scan-assembler "smaxv" } } */
/* { dg-final { scan-assembler "sminv" } } */
/* { dg-final { scan-assembler "sabd" } } */
/* { dg-final { scan-assembler "saba" } } */
/* { dg-final { scan-assembler-times "addv" 2} } */
/* { dg-final { scan-assembler-times "addp" 2} } */
