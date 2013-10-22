// { dg-options "-std=gnu++11" }
template<typename T1, typename T2>
float& f(T1, T2);

template<typename... Args>
int& f(Args...);

float& g() { 
  return f(17, 3.14159); 
}
