/* { dg-do compile } */
/* { dg-options "-O2 -std=c++11" } */

#define big __builtin_huge_val()
#define nan __builtin_nan("")

constexpr bool b1 = big > nan;
constexpr bool b2 = nan < big;

