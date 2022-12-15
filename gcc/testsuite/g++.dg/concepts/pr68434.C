// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template <class>
concept bool C1 () {
  return true;
}

template <class>
concept bool C2 () {
  return true;
}

template <class Expr>
concept bool C3 () {
  return requires (Expr expr) {
      {expr}->C1;
      {expr}->C2;
  };
}

auto f (C3);
