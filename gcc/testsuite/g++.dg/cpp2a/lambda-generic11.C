// PR c++/108999
// { dg-do compile { target c++20 } }

template<typename T>
void ice(T a) {
  auto aa = a;
  auto lambda = []<int I>() {
    if constexpr (sizeof(aa) + I != 42) { }
  };
  lambda.template operator()<17>();
}

template void ice(int);
