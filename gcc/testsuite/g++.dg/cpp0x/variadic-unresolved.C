// PR c++/50086
// { dg-options -std=c++11 }

template<typename T> void tfun();
template<typename T> void fun1(T);
template<typename... Types> void fun2(Types... args);

int main()
{
  fun1(tfun<int>); // ok
  fun2(tfun<int>); // error: unresolved overloaded function type
}
