// { dg-do compile { target c++11 } }

void bar();
void bar(int);

template <typename... Args>
void foo(Args... args) {
#if __cpp_init_captures >= 201803
  auto f = [...xs=args]{
    bar(xs...);
  };
#endif
}

int main()
{
  foo();
  foo(1);
}
