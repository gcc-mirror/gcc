struct S
{
  template<typename T> static void g();
};

template<typename T>
void f() { return S::template g<T>(); }

void g() {
  f<int>();
}
