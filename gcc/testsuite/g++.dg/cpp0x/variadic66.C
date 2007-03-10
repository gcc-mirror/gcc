// { dg-options "-std=gnu++0x" }

template<typename Result, typename Functor, typename... ArgTypes>
Result bind(Functor, ArgTypes...) { }

void f()
{
  bind<int>(17, 20, 22);
}
