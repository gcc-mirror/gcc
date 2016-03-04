// { dg-do compile { target c++14 } }

struct A
{
  int i;
};

constexpr bool f()
{
  A ar[4] = { 1, 2, 3, 4 };
  A *ap = ar;
  int i = 0;
  do
    *ap++ = A{i};
  while (++i < 3);
  return (ar[0].i == 0
	  && ar[1].i == 1
	  && ar[2].i == 2
	  && ar[3].i == 4);
}

#define SA(X) static_assert((X),#X)
SA(f());
