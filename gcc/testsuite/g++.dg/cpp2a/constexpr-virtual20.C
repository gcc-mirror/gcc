// Test for constexpr call through vbase thunk.
// { dg-do compile { target c++20 } }

class Rep {
public:
  constexpr virtual int foo() { return 1; }
};

class VBase {
public:
  constexpr virtual int foo() { return 2; }
};

class Main : public Rep, virtual public VBase {
public:
  constexpr virtual int foo() { return 5; }
};

int main() {
  Main m;
  static_assert(static_cast<VBase*>(&m)->foo() == 5); // { dg-error "" }
}
