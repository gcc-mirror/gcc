// PR c++/49672
// { dg-options -std=c++0x }

template<typename ... Args>
static void foo()
{
  [](Args..., int x) {
    x;
  };
}

int main()
{
  foo();
}
