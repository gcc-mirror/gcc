// { dg-options -std=c++11 }

enum class E { };
E f();
bool b2 = static_cast<bool>(f());
