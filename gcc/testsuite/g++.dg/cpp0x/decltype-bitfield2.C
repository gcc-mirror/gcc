// PR c++/95009
// { dg-do compile { target c++11 } }

struct A {
  int i:31;
};

template<typename>
void f ()
{ 
}

int main ()
{
  A a;
  f<decltype(a.i += 1)>();
  f<decltype(++a.i)>();
}
