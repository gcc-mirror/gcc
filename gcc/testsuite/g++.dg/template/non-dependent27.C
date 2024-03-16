// PR c++/111919

int i[42];

template<class T>
void f() {
  i[42 / (int)sizeof(T)] |= 42;
}
