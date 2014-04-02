// { dg-do compile { target c++11 } }
int& f(int, double, ...);

template<typename... Args>
float& f(Args...);

float& g() {
  return f(17, 3.14159, 3);
}
