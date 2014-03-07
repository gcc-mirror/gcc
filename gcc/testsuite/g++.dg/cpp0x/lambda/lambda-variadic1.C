// PR c++/49672
// { dg-do compile { target c++11 } }

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
