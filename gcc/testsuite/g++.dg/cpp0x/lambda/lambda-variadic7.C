// PR c++/86200
// { dg-do compile { target c++11 } }

template<typename ... Args>
static void foo()
{
  [](Args, int x) {
    x;
  };				// { dg-error "packs not expanded" }
}
int main()
{
  foo();
}
