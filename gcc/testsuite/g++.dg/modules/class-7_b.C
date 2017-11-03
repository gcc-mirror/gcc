
export module Two;
// { dg-module-bmi "Two" }
import One;

export struct middle : virtual base
{
  long long m;

  middle (int b_, int m_)
    : base (b_), m (m_)
  {
  }
};
