// PR c++/69694
// { dg-do compile { target c++11 } }

// n3911: TransformationTrait Alias `void_t`
template<typename...> struct make_void { using type = void; };
template<typename... Ts> using void_t = typename make_void<Ts...>::type;

// std::declval<void*&>
void*& declval_void();

template<typename, typename = void> struct Fun;
template<typename R>
  struct Fun<R(), void>
{
  void fun();
};
template<typename Desc>
  struct Fun<Desc, void_t<decltype (declval_void() = Desc::name)>>
    : Fun<void()>
{
};

struct Tag { static constexpr void* name = 0; };

template<typename> void a()
{
  Fun<Tag>{}.fun();
}

void b() { a<int>(); }
