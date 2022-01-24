// PR c++/60702
// { dg-do compile { target c++11 } }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }
// { dg-additional-options "-fdump-tree-gimple -fno-implicit-constexpr" }
// { dg-final { scan-tree-dump-times "_ZTW2s1" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTW2s2" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTW2s3" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTW2s4" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTWN1T2u1E" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTWN1T2u2E" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTWN1T2u3E" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTWN1T2u4E" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTWN1T2u5E" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTWN1T2u6E" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTWN1T2u7E" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTWN1T2u8E" 2 "gimple" } }

#include "thread_local11.h"

void
foo ()
{
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  f5 ();
  f6 ();
  f7<0> ();
  f8<0> ();
  f9<0> ();
  f10<0> ();
  f11<0> ();
  f12<0> ();
}
