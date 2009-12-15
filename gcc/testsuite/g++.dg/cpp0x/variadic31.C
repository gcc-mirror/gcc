// { dg-options "-std=gnu++0x -g" }
template<typename... T>
void eat(T...) { }

void f()
{
  eat();
  eat(1);
  eat(1, 2);
  eat(17, 3.14159, "Hello, World!");
}
