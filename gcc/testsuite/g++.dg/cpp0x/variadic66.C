// { dg-options "-std=gnu++11" }

template<typename Result, typename Functor, typename... ArgTypes>
Result bind(Functor, ArgTypes...) { }

void f()
{
  bind<int>(17, 20, 22);
}
