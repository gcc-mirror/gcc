// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T, typename... Params>
concept HasInit = requires(T t, Params... p) { t.init(p...); };

struct Initable { void init(int) { } };
struct Createable { void create(int) { } };

struct Foo{
  template<typename CB>
  void for_each(CB&& cb)
  {
    Initable i;
    Createable c;
    cb(i);
    cb(c);
  }

  Foo()
  {
    struct Bar { int x; };
    for_each(
	     [&](auto& foo){
	      if constexpr (HasInit<decltype(foo), int>)
                {
                    foo.init(5);
                }
	     });
  }
};

int main()
{
  Foo f;
  return 0;
}
