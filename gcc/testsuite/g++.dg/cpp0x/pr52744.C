// PR c++/52744
// { dg-do compile { target c++11 } }

struct T
{
  int a;
  void b(){}
  int c(int)
  {
    return 1;
    }
};

template<typename CT, CT> struct member_helper;

template<typename FT, FT(T::*mem)>
struct member_helper<FT(T::*), mem>
{
  static const char* worker()
  {
    return "for members";
  }
};

template<typename Return, typename... Args, Return(T::*fun)(Args...)>
struct member_helper<Return(T::*)(Args...), fun>
{
  static const char* worker()
  {
    return "for member functions returning non void";
  }
};

template<typename... Args, void(T::*fun)(Args...)>
struct member_helper<void(T::*)(Args...), fun>
{
  static const char* worker()
  {
    return "for member functions returning void";
  }
};

void member_test()
{
  member_helper<decltype(&T::a), &T::a>::worker();
  member_helper<decltype(&T::b), &T::b>::worker();
  member_helper<decltype(&T::c), &T::c>::worker();
}

typedef void lua_State;

template<typename T, T> class function_helper
{
  static_assert(sizeof(T) != sizeof(T),
		"Error: function_helper works with functions (duh)");
};

template<typename Return, typename... Args, Return(*func)(Args...)>
struct function_helper<Return(*)(Args...), func>
{
  static int wrapper(lua_State* l)
  {
    return 1;
  }
};

template<typename... Args, void(*func)(Args...)>
struct function_helper<void(*)(Args...), func>
{
  static int wrapper(lua_State* l)
  {
    return 0;
  }
};

int ciao(int){ return 0; }
void ciao2(int){}

void function_test()
{
  function_helper<decltype(&ciao), &ciao>::wrapper(0);
  function_helper<decltype(&ciao2), &ciao2>::wrapper(0);
}
