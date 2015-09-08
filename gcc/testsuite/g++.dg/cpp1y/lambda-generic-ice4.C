// PR c++/67369
// { dg-do compile { target c++14 } }

int main() {
  unsigned const nsz = 0;
  auto repeat_conditional = [&](auto) {
    auto new_sz = nsz;
  };
  repeat_conditional(1);
}
