// { dg-do compile { target c++2a } }

template <class>
concept C1 = true;

template <class>
concept C2 = true;

template <class Expr>
concept C3 =
  requires (Expr expr) {
      {expr}->C1;
      {expr}->C2;
  };

template<C3 T>
auto f (T);

