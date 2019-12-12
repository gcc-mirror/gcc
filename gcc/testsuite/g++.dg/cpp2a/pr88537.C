// { dg-do compile { target c++2a } }
// { dg-options "-g" }

struct pair {
	unsigned a;
	unsigned b;
	constexpr pair(unsigned _a, unsigned _b) noexcept: a{_a}, b{_b} { }
};

template <pair p> void fnc() {
	
}

void f() {
    fnc<pair(10,20)>();
}
