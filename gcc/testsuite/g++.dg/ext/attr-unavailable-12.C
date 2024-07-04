// PR c++/84542

template<class T>
void f(T);

template<class T>
__attribute__((unavailable))
void f(T) { }

int main() {
  f(0); // { dg-error "unavailable" }
}
