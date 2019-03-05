/* { dg-lto-do link } */
/* { dg-lto-options { "-O2 -fdump-ipa-devirt -flto -r -nostdlib" } } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
#include "../ipa/devirt-34.C"
/* { dg-final { scan-wpa-ipa-dump "Speculative targets"  "devirt"  } } */
/* { dg-final { scan-wpa-ipa-dump "1 speculatively devirtualized"  "devirt"  } } */
