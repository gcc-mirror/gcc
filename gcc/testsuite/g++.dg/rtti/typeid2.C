// { dg-do run }

#include <typeinfo>

template <typename T>  const char *print_type (const T &) {
  return typeid(T).name();
}

/* no template */      void pp1 (int) {}
template <typename X>  void pp2 (X)   {}

int main () {
  if (print_type (&pp1) != print_type (&pp2<int>))
    return 1;
}
