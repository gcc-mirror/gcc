// PR c++/116417
// { dg-do compile { target c++11 } }

template<class T>
T&& declval();

template<class T, class = decltype(declval<T>().~T())>
void f(int) = delete;

template<class T>
void f(...);

int main() {
  f<int&>(0);
}
