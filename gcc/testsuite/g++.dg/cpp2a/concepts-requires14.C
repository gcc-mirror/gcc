// { dg-do compile { target c++20 } }

template<typename T>
concept A = sizeof(T) >= 4;

template<typename T>
concept B = __is_class(T);

template<A T>
void ok1(T a) {
  return;
}

template<typename T>
  requires B<T>
void ok2(T a) {
  return;
}

template<A T>
  requires B<T>
void fun(T a) {
  return;
}
