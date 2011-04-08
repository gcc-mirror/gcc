// { dg-options -std=c++0x }

enum class E { };
E f();
bool b2 = static_cast<bool>(f());
