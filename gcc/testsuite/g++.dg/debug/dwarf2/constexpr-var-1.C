// { dg-do compile }
// { dg-options "-O -std=c++11 -g -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-times " DW_AT_const_expr" 2 { xfail *-*-aix* } } }

constexpr int a = 5;
struct S
{
  static constexpr int b = 6;
} s;
