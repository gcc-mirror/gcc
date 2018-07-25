// PR c++/86200
// { dg-do compile { target c++11 } }

template<typename ... Args>
static void foo()
{
  [](Args, int x) {		// { dg-error "packs not expanded" }
    x;
  };
}
int main()
{
  foo();
}
