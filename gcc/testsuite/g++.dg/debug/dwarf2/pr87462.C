// { dg-additional-options "-dA -std=gnu++17 -gdwarf-4 -O1 -fdebug-types-section" }
// reject .pseudo label, but "label" is ok.
// { dg-final { scan-assembler-not "\[^L\"\]_ZN5Test18testFuncEv" } }
// undefined ref to _ZN5Test18testFuncEv

class Test1 {
public:
  static int testFunc() { return 1; }
};

template <typename T,
          T (*funcImpl)()>
class TestWrapper {
public:
  static T func() __attribute((noinline)) { return (*funcImpl)(); } 
};

int main() {
  return TestWrapper<int, &Test1::testFunc>::func();
}
