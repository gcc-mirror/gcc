void f(int) {}
void f(double);

template <void (*fn)(int)>
void foo() {}

int main()
{
  foo<f>();
}

