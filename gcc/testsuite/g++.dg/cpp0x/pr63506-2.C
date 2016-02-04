// { dg-do compile { target c++11 } }

struct proxy {};

struct iterator
{
  proxy operator*() { return proxy(); }

  proxy operator[](int i) { return proxy(); }
};

//#define DEACTIVATE

#ifndef DEACTIVATE
template<typename T = int>
#endif
void foo(iterator it)
{
  auto&& x = *it;
  auto&& y = it[1];
}

int main()
{
  iterator it;
  foo(it);
}
