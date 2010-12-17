// { dg-do compile }
// { dg-prune-output "direct base" }
// { dg-options "-fdump-class-hierarchy" }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Dec 2002 <nathan@codesourcery.com>

// We ICE'd

struct c0 {};

struct c1 : virtual c0
{
  virtual c0 &f2() volatile;
};

struct c2 
{
  int m;
};

struct c3 : virtual c0, virtual c1, c2
{
  virtual c1 &f2() volatile;
};

struct c4 : virtual c3, virtual c0, virtual c1
{
  int m;
};

struct c6 : c0, c3, c4
{
  virtual c1 &f2() volatile;
};

// f2 appears four times in the c6 vtables:
// once in c1-in-c3-in-c6 - covariant, virtual base, uses c1 vcall offset and c0 vbase offset
// { dg-final { scan-tree-dump "24    .*c6::_ZTcv0_n16_v0_n12_NV2c62f2Ev" "class" { target ilp32 } } }
// { dg-final { scan-tree-dump "48    .*c6::_ZTcv0_n32_v0_n24_NV2c62f2Ev" "class" { target lp64 } } }
// once in c3-in-c6 - non-covariant, non-virtual base, calls f2 directly
// { dg-final { scan-tree-dump "28    .*c6::f2" "class" { target ilp32 } } }
// { dg-final { scan-tree-dump "56    .*c6::f2" "class" { target lp64 } } }
// once in c1-in-c3-in-c4-in-c6 - lost primary
// { dg-final { scan-tree-dump "80    .*0u" "class" { target ilp32 } } }
// { dg-final { scan-tree-dump "160   .*0u" "class" { target lp64 } } }
// once in c3-in-c4-in-c6 - c3 vcall offset
// { dg-final { scan-tree-dump "84    .*c6::_ZTv0_n16_NV2c62f2Ev" "class" { target ilp32 } } }
// { dg-final { scan-tree-dump "168   .*c6::_ZTv0_n32_NV2c62f2Ev" "class" { target lp64 } } }

// { dg-final { cleanup-tree-dump "class" } }
