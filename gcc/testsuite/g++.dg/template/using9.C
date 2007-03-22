// { dg-do compile }

// Origin: stefaandr@hotmail.com

// PR c++/17154: Using declaration in partial class template specialization.

template <int numrows, class T> struct A { void test_A() {} };
template <int numrows, class T> struct B {};
template <class T> struct B <3, T> : public A <3, T> {
	using A <3, T>::test_A;
	void test_B_spec() { test_A(); }
};
