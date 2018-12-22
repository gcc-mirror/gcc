/* { dg-lto-do link } */
/* { dg-lto-options { "-O2 -fdump-ipa-cp -Wno-return-type -flto -r -nostdlib" } } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
#include "../ipa/devirt-19.C"
/* { dg-final { scan-wpa-ipa-dump-times "Discovered a virtual call to a known target" 1 "cp"  } } */
