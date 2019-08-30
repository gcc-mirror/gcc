// PR c++/60702
// { dg-do compile { target c++11 } }
// { dg-add-options tls }
// { dg-require-alias "" }
// { dg-require-effective-target tls_runtime }
// { dg-additional-options "-fdump-tree-gimple" }
// { dg-final { scan-tree-dump-times "_ZTH2s1" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTH2s2" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTH2s3" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTH2s4" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTHN1T2u1E" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTHN1T2u2E" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTHN1T2u3E" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTHN1T2u4E" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTHN1T2u5E" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTHN1T2u6E" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTHN1T2u7E" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_ZTHN1T2u8E" 1 "gimple" } }

#include "thread_local11.C"
