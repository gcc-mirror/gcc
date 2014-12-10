// { dg-do compile { target c++14 } }

constexpr int f (int i)
{
  int j = 0;
  switch (i)
    {
    case 1:
      j = 42;
      break;
    default:
      j = 24;
      break;
    }
  return j;
}

constexpr int i = f(2);
#define SA(X) static_assert((X),#X)
SA(i==24);
