// PR c++/114992
// { dg-do compile { target { c++11 && x86_64-*-* } } }
// { dg-require-ifunc "" }

template <typename Callable>
__attribute__((target_clones("avx2", "default")))
void handler(Callable) {}

inline int func()
{
  auto l1 = [](int) {}; // different lambda signature
  handler([]() {}); // so this one needs a mangling alias
  return 42;
}

int g = func();
