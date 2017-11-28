// { dg-do compile { target c++11 } }

template<typename Result, typename Functor, typename... ArgTypes>
Result bind(Functor, ArgTypes...) { return Result(); }

void f()
{
  bind<int>(17, 20, 22);
}
