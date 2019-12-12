// PR c++/91705 - constexpr evaluation rejects ++/-- on floats.
// { dg-do compile { target c++14 } }

#define SA(X) static_assert((X),#X)

template <class T>
constexpr T fn1(T t)
{
  return ++t;
}

constexpr float fn2(float t)
{
  return ++t;
}

template <class T>
constexpr T fn3(T t)
{
  return --t;
}

constexpr float fn4(float t)
{
  return --t;
}

template <class T>
constexpr T fn5(T t)
{
  return t++;
}

constexpr float fn6(float t)
{
  return t++;
}

template <class T>
constexpr T fn7(T t)
{
  return t--;
}

constexpr float fn8(float t)
{
  return t--;
}

constexpr double r1 = fn1(2.0f);
SA(r1 == 3);
constexpr double r2 = fn2(2.0f);
SA(r2 == 3);
constexpr double r3 = fn3(2.0f);
SA(r3 == 1);
constexpr double r4 = fn4(2.0f);
SA(r4 == 1);

constexpr double r5 = fn5(2.0f);
SA(r5 == 2);
constexpr double r6 = fn6(2.0f);
SA(r6 == 2);
constexpr double r7 = fn7(2.0f);
SA(r7 == 2);
constexpr double r8 = fn8(2.0f);
SA(r8 == 2);
