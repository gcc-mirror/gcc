// PR c++/43641
// { dg-options "-std=c++0x" }

struct B
{
  int i;
};

void func()
{
  [](const B& b) -> const int& { return b.i; };
  [](const B& b) { return b; };
}
