// PR c++/57041
// { dg-do compile { target c++11 } }
// { dg-options "" }

template<typename T>
union u {
  T a;
  char b;
};

template<typename T>
u<T> make_u(T t) {
  return { .a = t };
}

int main() {
  return make_u<int>(1).a;
}
