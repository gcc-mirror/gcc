// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

template<typename T>
[[z]]
[[nodiscard]]
T fun(T n)
  [[ pre: n > 0 ]]
  [[ post r: r > 0 ]] // { dg-warning ".z. attribute.*ignored" }
{
  return n;
}

int main(int, char**) {
  fun(-5); // { dg-warning "ignoring return value" }
  fun(-5.3); // { dg-warning "ignoring return value" }
  return 0;
}

