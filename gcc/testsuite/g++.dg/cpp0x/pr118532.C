// PR c++/118532
// { dg-do compile { target c++11 } }

struct S {
  S (int, int, int);
#define I8 int, int, int, int, int, int, int, int
#define I64 I8, I8, I8, I8, I8, I8, I8, I8
  S (I64, I64, I64, I64, I8);
};

void
foo (S &)
{
}

int
main ()
{
  S s = {
#undef I8
#define I8 1, 2, 3, 4, 5, 6, 7, 8
    I64, I64, I64, I64, I8
  };
  foo (s);
}
