/* { dg-do compile { target int128 } } */
/* { dg-options "-std=c11" } */
/* { dg-final { scan-assembler-times {\.align\t2} 2 } } */
/* { dg-final { scan-assembler-times {\.align\t4} 2 } } */
/* { dg-final { scan-assembler-times {\.align\t8} 3 } } */
/* { dg-final { scan-assembler-times {\.align\t16} 2 } } */

// 2
_Atomic char var_char;
_Atomic short var_short;
// 4
_Atomic int var_int;
// 8
_Atomic long var_long;
_Atomic long long var_long_long;
// 16
_Atomic __int128 var_int128;
// 4
_Atomic float var_float;
// 8
_Atomic double var_double;
// 16
_Atomic long double var_long_double;
