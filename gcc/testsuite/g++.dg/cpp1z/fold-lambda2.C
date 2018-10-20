// PR c++/85305
// { dg-do compile { target c++17 } }

template <int... Is>
void foo()
{
  ([i = Is]{}(), ...); 
}
