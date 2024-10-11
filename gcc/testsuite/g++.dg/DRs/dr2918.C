// CWG 2918 makes this OK
// { dg-do compile { target c++20 } }

template<bool B> struct X {
  static void f(short) requires B; // #1
  static void f(short);		   // #2
};
void test() {
  auto x = &X<true>::f;		// OK, deduces void(*)(short), selects #1
  auto y = &X<false>::f;	// OK, deduces void(*)(short), selects #2
}
