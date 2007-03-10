// { dg-options "-std=gnu++0x" }
template<typename T, typename... Args>
int& f(const T&, Args...);

template<typename T>
float& f(const T&);

float& g() {
  return f(17);
}
