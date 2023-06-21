// PR c++/105221
// { dg-do compile { target c++14 } }

void (*p)(int) = [](auto) noexcept {};

int main() {
  true ? [](auto) noexcept {} : [](int) {};
}
