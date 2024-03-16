// PR c++/111929

template<class>
void f(char x) {
  new int[x + 42];
}
