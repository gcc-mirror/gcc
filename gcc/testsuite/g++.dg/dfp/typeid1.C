// PR c++/39131
// { dg-do link }

#include <typeinfo>

const std::type_info &r = typeid(0.dd);

int main() { }
