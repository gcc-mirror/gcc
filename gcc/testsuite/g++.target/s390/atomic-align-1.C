/* { dg-do compile { target int128 } } */
/* { dg-options "-std=c++11" } */
/* { dg-final { scan-assembler-times {\.align\t2} 2 } } */
/* { dg-final { scan-assembler-times {\.align\t4} 2 } } */
/* { dg-final { scan-assembler-times {\.align\t8} 3 } } */
/* { dg-final { scan-assembler-times {\.align\t16} 2 } } */

#include <atomic>

// 2
std::atomic<char> var_char;
std::atomic<short> var_short;
// 4
std::atomic<int> var_int;
// 8
std::atomic<long> var_long;
std::atomic<long long> var_long_long;
// 16
std::atomic<__int128> var_int128;
// 4
std::atomic<float> var_float;
// 8
std::atomic<double> var_double;
// 16
std::atomic<long double> var_long_double;
