// PR c++/95298
// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-compat-version=0 }

template<class...>
struct list{};

template<int n>
struct _func_select
{
  using f = void;
};

struct func
{
  template<class... seqs>
  using f = typename _func_select<sizeof...(seqs)>::f;
};

template<class... T>
func::f<list<T>...> foo(T&&...)
{}

// { dg-final { scan-assembler "_Z3fooIJEEN12_func_selectIXsPDp4listIJT_EEEEE1fEDpOS2_" } }

int main()
{
  foo();
}
