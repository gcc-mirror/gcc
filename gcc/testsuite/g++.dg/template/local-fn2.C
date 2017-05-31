// PR c++/80856
// { dg-do compile { target c++11 } }

template<typename T>
inline T WrapToCycle(T degrees)
{
  int Wrap(int x, int lower_bound, int upper_bound);

  auto p = Wrap;
  p (1, 0, 360);

  double Wrap(double x, int lower_bound, int upper_bound);

  Wrap(1, 0, 360);
  return Wrap(degrees, 0, 360);
}

void GenerateOldReportPage()
{
  WrapToCycle(0);
}
