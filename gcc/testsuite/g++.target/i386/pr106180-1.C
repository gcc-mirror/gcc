/* { dg-do compile } */
/* { dg-options "-O3 -c -ffloat-store  -std=c++11" } */

struct PointT 
{
  double x, y;
};
using PointF = PointT;

template <int _Nm> struct __array_traits { typedef PointT _Type[_Nm]; };
template <int _Nm> struct array
{
  typename __array_traits<_Nm>::_Type _M_elems;
};

float SampleGrid_low, SampleGrid_high;
using QuadrilateralF = array<4>;
struct PerspectiveTransform
{
  PerspectiveTransform (QuadrilateralF, QuadrilateralF);
};

void SampleGrid()
{
  PerspectiveTransform
  {
    { PointF {SampleGrid_high, SampleGrid_low},
      SampleGrid_low, SampleGrid_high },
    {}
  };
}
