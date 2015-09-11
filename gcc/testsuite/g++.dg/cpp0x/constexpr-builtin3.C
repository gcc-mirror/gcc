// PR c++/65656
// { dg-options "-std=c++11 -O" }

int main(int argc, char *argv[]) {
  constexpr bool x = __builtin_constant_p(argc);
}
