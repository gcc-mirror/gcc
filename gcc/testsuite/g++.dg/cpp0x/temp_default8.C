// PR c++/67898
// { dg-do compile { target c++11 } }

void f(int);
void f(float);

template<class T, T F, T G, bool b = F == G> struct X { };

template<class T>
void test() {
  X<void(T), f, f>();
}

template void test<int>();
