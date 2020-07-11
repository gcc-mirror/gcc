// { dg-do compile { target c++20 } }

template<typename T>
concept Class = __is_class(T);

template<Class T>
void f1(T) { }

struct empty { };

// Redeclarations involving brief template parameters.

template<Class T>
void decl1(T);

template<typename T>
  requires Class<T>
void decl1(T);

void driver_1()
{
  f1(0); // { dg-error "" }
  f1(empty{});

  decl1(empty{}); // { dg-error "call of overload | ambiguous" }
}

