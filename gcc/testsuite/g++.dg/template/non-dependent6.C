// PR c++/15285

void foo(void (*func)()) {}

template<typename T>
void bar()
{}

template<typename T>
void baz()
{
  foo(&bar<long>);
}
