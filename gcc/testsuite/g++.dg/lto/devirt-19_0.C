/* { dg-lto-do link } */
/* { dg-lto-options { "-O2 -fdump-ipa-cp -fipa-cp-clone -Wno-return-type -flto -r -nostdlib" } } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel -flto=auto" } */
#include "../ipa/devirt-19.C"
/* { dg-final { scan-wpa-ipa-dump-times "Discovered a virtual call to a known target" 1 "cp"  } } */
