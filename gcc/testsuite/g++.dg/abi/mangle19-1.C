// { dg-do compile }
// { dg-options "-fabi-version=2 -Wno-abi" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Dec 2003 <nathan@codesourcery.com>

// PR 13242
// mangled template arguments that are external objects incorrectly

extern int N;
template <int &> struct S {};
void n (S<N>) {}
// { dg-final { scan-assembler "\n_?_Z1n1SILZ1NEE\[: \t\n\]" } }
