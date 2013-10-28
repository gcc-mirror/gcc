// { dg-options "-std=gnu++11" }
template<typename T, typename... Args>
int& f(const T&, Args...);

template<typename T>
float& f(const T&);

float& g() {
  return f(17);
}
