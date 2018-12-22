/* { dg-lto-do run } */
/* { dg-lto-options { "-O3 -fno-early-inlining -fno-inline -fdump-ipa-cp -fdump-tree-optimized -flto" } } */
#include "../ipa/devirt-2.C"
/* { dg-final { scan-wpa-ipa-dump "Discovered a virtual call to a known target.*foo"  "cp"  } } */
