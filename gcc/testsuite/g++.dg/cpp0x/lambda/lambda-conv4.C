// PR c++/43641
// { dg-do compile { target c++11 } }

struct B
{
  int i;
};

void func()
{
  [](const B& b) -> const int& { return b.i; };
  [](const B& b) { return b; };
}
