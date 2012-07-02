// PR c++/53821
// { dg-final { scan-assembler-not "_ZZ1fIvEvvENKUlvE_cvPFvvEEv" } }
// { dg-do compile { target c++11 } }

template <class T> void f()
{
  auto g = []{};
  g();
}

int main()
{
  f<void>();
}
