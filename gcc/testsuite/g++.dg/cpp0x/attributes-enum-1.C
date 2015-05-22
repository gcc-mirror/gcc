// PR c/47043
// { dg-do compile { target c++11 } }

enum E {
  A [[gnu::deprecated]]
};

enum class F {
  B [[gnu::deprecated]],
  C __attribute__ ((deprecated))
};

int
f (int i)
{
  F f1 = F::B; // { dg-warning ".B. is deprecated" }
  F f2 = F::C; // { dg-warning ".C. is deprecated" }
  i += A; // { dg-warning ".A. is deprecated" }
  return i;
}
