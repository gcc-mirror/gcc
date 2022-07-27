/* { dg-options "-w -Wno-abi" } */
/* { dg-options "-w -mno-mmx -Wno-abi" { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-w -fno-common" { target hppa*-*-hpux* powerpc*-*-darwin* } } */
/* { dg-options "-w -mno-mmx -fno-common -Wno-abi" { target i?86-*-darwin* x86_64-*-darwin* } } */
/* { dg-options "-w -mno-base-addresses" { target mmix-*-* } } */
/* { dg-options "-w -mlongcalls -mtext-section-literals" { target xtensa*-*-* } } */
#include "struct-layout-1_y1.h"
#include "pr102024_test.h"
#include "struct-layout-1_y2.h"
#include "pr102024_test.h"
