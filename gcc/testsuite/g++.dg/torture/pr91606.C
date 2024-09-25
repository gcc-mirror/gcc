/* { dg-do run } */
/* { dg-additional-options "-fstrict-aliasing" } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib size_t" { ! hostedlib } } */

#include <cstdlib>
#include <array>
#include <type_traits>

template <typename T1, typename T2>
struct variant
{
  constexpr variant(T1 arg)
      : f1(arg),
      index(0)
  {}

  constexpr variant(T2 arg)
      : f2(arg),
      index(1)
  {}

  union
    {
      T1 f1;
      T2 f2;
    };
  std::size_t index = 0;
};

template <typename T1, typename T2>
constexpr const T1* get_if(const variant<T1, T2>* v)
{
  if (v->index != 0)
    {
      return nullptr;
    }
  return &v->f1;
}

template <typename T2, typename T1>
constexpr const T2* get_if(const variant<T1, T2>* v)
{
  if (v->index != 1)
    {
      return nullptr;
    }
  return &v->f2;
}

template <typename T, size_t N>
struct my_array
{
  constexpr const T* begin() const
    {
      return data;
    }

  constexpr const T* end() const
    {
      return data + N;
    }

  T data[N];
};

template <typename ...Ts>
constexpr auto get_array_of_variants(Ts ...ptrs)
{
  return std::array<variant<std::decay_t<Ts>...>, sizeof...(Ts)>{ ptrs... };
}

template <typename T>
constexpr auto get_member_functions();

template <typename Member, typename Class>
constexpr int getFuncId(Member (Class::*memFuncPtr))
{
  int idx = 0u;
  for (auto &anyFunc : get_member_functions<Class>())
    {
      if (auto *specificFunc = get_if<Member (Class::*)>(&anyFunc))
	{
	  if (*specificFunc == memFuncPtr)
	    {
	      return idx;
	    }
	}
      ++idx;
    }
  std::abort();
}

struct MyStruct
{
  void fun1(int /*a*/) {}

  int fun2(char /*b*/, short /*c*/, bool /*d*/) { return 0; }

};

template <>
constexpr auto get_member_functions<MyStruct>()
{
  return get_array_of_variants(&MyStruct::fun1, &MyStruct::fun2);
}

int main()
{
  return getFuncId(&MyStruct::fun1);
}
