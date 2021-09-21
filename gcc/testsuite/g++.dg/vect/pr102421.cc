/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-march=armv8.2-a+sve" { target aarch64-*-* } } */

#include <vector>

template<typename ValueType>
struct BasicVector
{
    ValueType& operator[](int i) { return x_[i]; }
    ValueType operator[](int i) const { return x_[i]; }
    ValueType x_[3];
};
typedef int ivec1[3];
typedef BasicVector<double> RVec1;
void foo (
   std::vector<RVec1> &x_,
   std::vector<RVec1> &xp_,
   int homenr,
   unsigned short* cFREEZE,
   const ivec1* nFreeze)
{
    std::vector<RVec1> xp = xp_;
    std::vector<RVec1> x = x_;
    for (int i = 0; i < homenr; i++)
    {
        const int g = cFREEZE[i];
        for (int d = 0; d < 3; d++)
        {
            if (nFreeze[g][d] == 0)
                x[i][d] = xp[i][d];
        }
    }
}
