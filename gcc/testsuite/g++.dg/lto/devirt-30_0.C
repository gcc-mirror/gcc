/* { dg-lto-do link } */
/* { dg-lto-options { "-O3 -fdump-ipa-devirt -flto -r -nostdlib" } } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
#include "../ipa/devirt-30.C"
// { dg-final { scan-wpa-ipa-dump-not "Speculatively devirtualizing" "devirt" } }
