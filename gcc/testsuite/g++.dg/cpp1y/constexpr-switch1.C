// { dg-do compile { target c++14 } }

constexpr int f (int i)
{
  switch (i)
    {
    case 1:
      return 42;
    default:
      return 0;
    }
}

constexpr int i = f(1);
#define SA(X) static_assert((X),#X)
SA(i==42);
