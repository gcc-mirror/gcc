// { dg-options "-std=gnu++0x" }
int& f(int, double, ...);

template<typename... Args>
float& f(Args...);

float& g() {
  return f(17, 3.14159, 3);
}
