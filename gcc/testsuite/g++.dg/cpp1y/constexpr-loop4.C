// { dg-do compile { target c++14 } }

struct A
{
  int i;
};

constexpr bool f()
{
  A ar[5] = { 6, 7, 8, 9, 10 };
  A *ap = ar;
  int i = 0, j = 0;
  for (j = 0; j < 2; j++)
    {
      do
	*ap++ = A{i};
      while (++i < j * 2 + 2);
    }
  return (ar[0].i == 0
	  && ar[1].i == 1
	  && ar[2].i == 2
	  && ar[3].i == 3
	  && ar[4].i == 10);
}

#define SA(X) static_assert((X),#X)
SA(f());
