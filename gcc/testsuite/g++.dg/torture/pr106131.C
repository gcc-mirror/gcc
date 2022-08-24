// { dg-do run { target c++11 } }

struct Pair {
    int a, b;
    Pair(const Pair &) = default;
    Pair(int _a, int _b) : a(_a), b(_b) {}
    Pair &operator=(const Pair &z) {
	a = z.a;
	b = z.b;
	return *this;
    }
};

const int &max(const int &a, const int &b)
{
  return a < b ? b : a;
}

int foo(Pair x, Pair y)
{
  return max(x.b, y.b);
}

int main()
{
  auto f = new Pair[3] {{0, -11}, {0, -8}, {0, 2}};
  for (int i = 0; i < 1; i++) {
      f[i] = f[0];
      if(i == 0)
	f[i] = f[2];
      if (foo(f[i], f[1]) != 2)
	__builtin_abort();
  }
}
