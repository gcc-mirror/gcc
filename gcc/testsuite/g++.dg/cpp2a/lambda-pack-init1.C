// { dg-do compile { target c++20 } }

void bar();
void bar(int);

template <typename... Args>
void foo(Args... args) {
  [...xs=args]{
    bar(xs...); // xs is an init-capture pack
  };
}

int main()
{
  foo();  // OK: xs contains zero init-captures
  foo(1); // OK: xs contains one init-capture
}
