/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra -ffinite-loops"  } */
#include "ipa-sra-30.c"
/* { dg-final { scan-ipa-dump "Created new node kill.isra"  "sra"  } } */
