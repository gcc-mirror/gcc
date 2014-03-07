// { dg-do compile { target c++11 } }
template<typename... Args>
int& f(Args...);

template<typename T1, typename T2>
float& f(T1, T2);

float& g() { 
  return f(17, 3.14159); 
}
