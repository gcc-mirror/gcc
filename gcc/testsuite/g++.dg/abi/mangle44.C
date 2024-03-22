// PR c++/45008
// { dg-additional-options -fabi-compat-version=0 }

template <typename T>
struct A
{
  void fn1 () {
   struct Nested {
    static void fn2 () { }
   };
   Nested::fn2();
  }
};

void fn3 () {
  A<double> a;
  a.fn1();
}

// { dg-final { scan-assembler-not "_ZZN1AIT_E3fn1EvEN6Nested3fn2Ev" } }
