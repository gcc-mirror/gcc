struct B {
  B(); // This type is non-literal
  void func();
};

template <class Class, void (Class::*Ptr)()>
void match();

template <class Any, Any Value>
void match();

void func() {
  match<B, &B::func>();
}
