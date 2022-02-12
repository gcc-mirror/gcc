// PR c++/103752
// { dg-additional-options -fmodule-header }
// { dg-require-effective-target c++20 }

template<typename _T1, typename _T2>
struct pair
{
        constexpr
      explicit(__is_same(_T1, _T2))
      pair()
      { }

        _T1 first;
        _T2 second;
};

struct string
{
  string() { }
  string(const char* s) : s(s) { }

  const char* s = "";
};
