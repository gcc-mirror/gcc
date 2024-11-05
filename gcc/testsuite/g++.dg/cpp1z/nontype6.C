// PR c++/101463
// { dg-do compile { target c++17 } }

int a;

int& v = a;

template<const int& = v>
void f(int) { }

template<class T, int& = v>
void g(T) { }

template<class T>
int& vt = a;

template<class T, int& = vt<T>>
void h(T) { }

int main() {
  f(0);
  g(0);
  h(0);
}
