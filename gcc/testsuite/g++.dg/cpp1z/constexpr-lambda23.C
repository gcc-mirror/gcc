// PR c++/86926
// { dg-do compile { target c++17 } }

int
main()
{
    constexpr auto f = [](auto self, auto n) {
        if(n < 2)
	  return n;
        return self(self, n - 1) + self(self, n - 2);
    };

    constexpr auto fibonacci = [=](auto n) { return f(f, n); };

    static_assert(fibonacci(7) == 13);
}
