// { dg-do run { target c++11 } }
template <typename T, T... Args> struct bomb;

template <typename T>
struct bomb<T> {
 static const T value = 0;
};

template <typename T, T v, T... Args>
struct bomb<T, v, Args...> {
 static const T value = v + bomb<T, Args...>::value;
};

extern "C" void abort();

int main() {
  bomb<int, 1, 2, 3, 4> b;
  if (b.value != 10)
    abort();
  return 0;
}
