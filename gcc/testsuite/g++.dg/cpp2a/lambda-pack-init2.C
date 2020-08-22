// PR c++/89686
// { dg-do compile { target c++20 } }

template <typename... Ts>
void foo(Ts... xs)
{
  int i = 10;
  [...xs...]{}(); // { dg-error "4:too many ..... in lambda capture" }
  [...xs...=xs]{}(); // { dg-error "9:too many ..... in lambda capture|expected" }
  [xs...]{}();
  [...xs=xs]{}();

  [i, ...xs...]{}(); // { dg-error "7:too many ..... in lambda capture" }
  [i, ...xs...=xs]{}(); // { dg-error "12:too many ..... in lambda capture|expected" }
  [i, xs...]{}();
  [i, ...xs=xs]{}();
}

int main()
{
 foo(0, 1, 2);
}
