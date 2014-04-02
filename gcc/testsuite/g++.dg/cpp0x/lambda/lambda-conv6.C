// PR c++/55015
// { dg-do link { target c++11 } }

typedef void (*VoidFunc)();
inline VoidFunc GetFunc() { return [](){}; }
int main() { VoidFunc func = GetFunc(); }
