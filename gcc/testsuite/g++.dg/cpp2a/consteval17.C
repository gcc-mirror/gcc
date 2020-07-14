// { dg-do compile { target c++20 } }

struct A
{
  consteval int operator+() { return 42; }
};

int main()
{
  +A();
}
