/* { dg-lto-do run } */
/* { dg-lto-options { "-O3 -fno-early-inlining -fno-ipa-sra -fdump-ipa-cp -flto -fno-devirtualize-speculatively" } } */
#include "../ipa/devirt-23.C"
/* { dg-final { scan-wpa-ipa-dump "Discovered a virtual call to" "cp" { xfail *-*-* } } } */
