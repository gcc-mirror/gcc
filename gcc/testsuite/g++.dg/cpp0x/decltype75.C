// PR c++/69694
// This is a reduced version of decltype74.C.
// { dg-do compile { target c++11 } }

template<typename... Ts> using void_t = void;

extern void *declval_void;

template<typename, typename> struct Fun { };

template<typename Desc>
struct Fun<Desc, void_t<decltype (declval_void = Desc::name)>>
{
  void fun();
};

struct Tag { static constexpr void* name = 0; };

template<typename> void a()
{
  Fun<Tag, void>{}.fun();
}

void b() { a<int>(); }
