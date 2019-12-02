// { dg-lto-do link }
/* { dg-lto-options { "-O2 -fno-early-inlining -flto -fdump-ipa-inline" } } */
#include "inline-crossmodule-1.h"
int a::key ()
{
  return 0;
}
/* { dg-final { scan-wpa-ipa-dump-times "Inlined ret1" 1 "inlined"  } } */
/* { dg-final { scan-wpa-ipa-dump-times "Inlined ret2" 1 "inlined"  } } */
/* { dg-final { scan-wpa-ipa-dump-times "Inlined key\[^\\n\]*(cross module)" 1 "inlined"  } } */
/* { dg-final { scan-wpa-ipa-dump-times "(cross module)" 1 "inlined"  } } */
