// { dg-do compile { target c++2a } }

template<class T>
bool f(T x) {
  return requires(T x) {
    ++x;
  };
}

int main() {
  f(3);
  return 0;
}
