// { dg-do compile { target c++11 } }

struct proxy {};

struct iterator
{
  proxy operator*() { return proxy(); }
};

//#define DEACTIVATE

#ifndef DEACTIVATE
template<typename T = int>
#endif
void foo(iterator it)
{
  auto&& x = *it;
}

int main() 
{
  iterator it;
  foo(it);
}
