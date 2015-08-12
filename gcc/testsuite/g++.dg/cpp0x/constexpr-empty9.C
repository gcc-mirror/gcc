// PR c++/65896
// { dg-do compile { target c++11 } }

struct base {};

struct derived :  base {
	constexpr derived():
		base{},
		m_value(0) {
	}
	int m_value;
};

constexpr int by_ref(derived && value) {
	return value.m_value;
}

constexpr int value = by_ref(derived{});
