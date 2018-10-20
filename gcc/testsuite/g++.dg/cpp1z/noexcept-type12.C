// { dg-do compile { target c++17 } }

template <class R, class... A, bool B>
void f(R (*)(A...) noexcept(B)) { }

template <class R, class... A, bool B>
void f2(R (*)(A...) noexcept(B)) { }

void g(int);
void h(int) noexcept;

int main()
{
  f(g);
  f2(h);
}

// { dg-final { scan-assembler "_Z1fIvJiELb0EEvPDOT1_EFT_DpT0_E" } }
// { dg-final { scan-assembler "_Z2f2IvJiELb1EEvPDOT1_EFT_DpT0_E" } }

void f3(void (*)() noexcept) { }

// { dg-final { scan-assembler "_Z2f3PDoFvvE" } }
