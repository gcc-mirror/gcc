// PR c++/118532
// { dg-do compile { target c++20 } }
// { dg-options "" }

constexpr int fn0 () { return 0; }
constexpr int fn1 () { return 1; }

struct S {
  explicit(fn0()) S(int, int, int);
#define I8 int, int, int, int, int, int, int, int
#define I64 I8, I8, I8, I8, I8, I8, I8, I8
  explicit(fn1()) S(I64, I64, I64, I64, I8);
};

int
main ()
{
  S s4 = {
#undef I8
#define I8 1, 2, 3, 4, 5, 6, 7, 8
    I64, I64, I64, I64, I8
  }; // { dg-error "converting" }
}
