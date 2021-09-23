/* PR 82800 - Incorrect warning on "may be used uninitialized in
   variadic template code
   { dg-do compile { target c++11 } }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern "C" int rand ();

struct Maker
{
  double makeConst()
  {
    return pick<double>(0, 0, 0, 0, 0, 0, 1);   // { dg-bogus "uninitialized" }
  }

  template<typename T, typename... Args>
  T pick(T first, Args... args)
  {
    return pickGivenNum<T>(rand(), first, args...);
  }

  template<typename T>
  T pickGivenNum(size_t num, T first)
  {
    if (num != 0) __builtin_abort();
    return first;
  }

  template<typename T, typename... Args>
  T pickGivenNum(size_t num, T first, Args... args)
  {
    if (num == 0) return first;
    return pickGivenNum<T>(num - 1, args...);
  }
};

int main ()
{
  Maker maker;
  maker.makeConst();
  maker.makeConst();
}
