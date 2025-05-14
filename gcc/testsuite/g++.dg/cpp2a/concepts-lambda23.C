// PR c++/119175
// { dg-do compile { target c++20 } }

template<int = 0>
static void from() requires requires {
  []<int> requires requires { [] {}; } {};
}
{}

int main() {
  from();
}
