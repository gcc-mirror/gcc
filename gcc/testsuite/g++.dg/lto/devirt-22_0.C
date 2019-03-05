/* { dg-lto-do link } */
/* { dg-lto-options { "-O3 -fno-early-inlining -fno-ipa-sra -fdump-ipa-cp -flto -r -nostdlib" } } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
#include "../ipa/devirt-22.C"
/* { dg-final { scan-wpa-ipa-dump-times "Discovered a virtual call to a known target" 2 "cp"  } } */
