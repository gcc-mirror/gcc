// PR c++/41927
// { dg-options "-std=c++0x -Wall" }

// We were getting a spurious ||/&& warning about the enable_if with the
// source position of d1.

template<typename Tp>
  struct is_int
  { static const bool value = true; };

template<bool, typename Tp = void>
  struct enable_if
  { };

template<typename Tp>
  struct enable_if<true, Tp>
  { typedef Tp type; };

template<typename Rep>
  struct duration
  {
    duration() { }

    template<typename Rep2, typename = typename
             enable_if<false || (true && is_int<Rep2>::value)>::type>
    duration(const duration<Rep2>&) { }
  };

int main()
{
  duration<int> d0;
  duration<int> d1 = d0;
}

