/* { dg-lto-do run } */
/* { dg-lto-options {{ -O2 -fno-strict-aliasing -flto }} } */

template <class T>
struct pair
{
    int first;
    T second;
};

template <typename C>
[[gnu::optimize("strict-aliasing")]]
bool __attribute__((noinline))
compare_pairs(const pair<C> &lhs, const pair<C> &rhs) {
  return lhs.first == rhs.first && lhs.second == rhs.second;
}

template <typename B> struct Combined {
  pair<B *> 
__attribute__((noinline)) get_const() {
    return pair<B *>{123, nullptr};
  }
[[gnu::optimize("strict-aliasing")]]
  bool 
__attribute__((noinline)) clashy() {
    return compare_pairs(get_const(), get_const());
  }
};

class SomeClass {};
class OtherClass {};

[[gnu::optimize("strict-aliasing")]]
[[gnu::used]]
void some_func() {
  Combined<SomeClass> myvar;
  __builtin_printf("%i\n", myvar.clashy());
}

[[gnu::optimize("strict-aliasing")]]
void other_func() {
  Combined<OtherClass> myvar;
  int t = myvar.clashy();
  if (!t)
  __builtin_abort();
}

[[gnu::optimize("O0")]]
int main()
{
  other_func();
}
