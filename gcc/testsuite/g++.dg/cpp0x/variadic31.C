// { dg-options "-g" }
// { dg-do compile { target c++11 } }
template<typename... T>
void eat(T...) { }

void f()
{
  eat();
  eat(1);
  eat(1, 2);
  eat(17, 3.14159, "Hello, World!");
}
