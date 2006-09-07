// PR c++/26195
extern void foo1();
extern void foo2();

int main()
{
  foo1();
  foo2();
}
