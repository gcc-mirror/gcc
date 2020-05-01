// PR 94287 ICE looking inside open template-parm level
// { dg-do run { target c++17 } }
// { dg-options -fconcepts }

template <typename T,
  bool X = requires { requires (sizeof(T)==1); } >
  int foo(T) { return X; }

int main() {
  if (!foo('4'))
    return 1;
  if (foo (4))
    return 2;
  return 0;
}
