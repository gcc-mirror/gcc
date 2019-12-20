// PR c++/92965
// { dg-do compile { target c++2a } }

template<int>
class TS {
  int x;	// { dg-bogus "is not public" }
public:
  constexpr TS(int) {}
};
TS(int) -> TS<1>;

template<TS> void foo() {}
template<int> void foo() {}

void test() { foo<2>(); }
