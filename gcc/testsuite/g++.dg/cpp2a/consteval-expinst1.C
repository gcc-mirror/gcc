// PR c++/96905
// { dg-do compile { target c++20 } }

template<typename Rep>
struct duration
{
  static consteval int
  gcd(int m, int n) noexcept
  {
    while (m != 0 && n != 0)
    {
      int rem = m % n;
      m = n;
      n = rem;
    }
    return m + n;
  }
};

template class duration<int>;
