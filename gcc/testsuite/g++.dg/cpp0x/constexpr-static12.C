// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "_ZNSt10unique_ptrC1Ei" } }

namespace std {
  struct unique_ptr {
    constexpr unique_ptr(int) : p() { }
    ~unique_ptr() { }
    void* p;
  };
}

void f()
{
  static std::unique_ptr p(1);
}

int main()
{
  f();
}
