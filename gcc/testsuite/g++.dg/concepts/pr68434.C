// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

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


auto f (C3 auto);
