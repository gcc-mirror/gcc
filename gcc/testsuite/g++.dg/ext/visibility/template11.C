// PR c++/51930
// { dg-require-visibility "" }
// { dg-options "-fvisibility=hidden" }
// { dg-final { scan-not-hidden "_ZN13template_testI4testE8functionEv" } }

struct test { };

template<typename T>
struct template_test
{
  __attribute__((visibility("default")))
  void function();
};

template<typename T>
void template_test<T>::function() { }

template
struct __attribute__((visibility("default")))
template_test<test>;
