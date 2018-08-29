// PR c++/85305
// { dg-additional-options -std=c++17 }

template <int... Is>
void foo()
{
  ([i = Is]{}(), ...); 
}
