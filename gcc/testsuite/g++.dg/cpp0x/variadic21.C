// { dg-options "-std=gnu++0x" }
template<typename T, int... Dims>
struct array { };

array<int> a0;
array<int, 1> a1;
array<int, 1, 2, 3, 4> a1234;
