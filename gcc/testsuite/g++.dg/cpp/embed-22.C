// PR c++/118532
// { dg-do compile { target c++11 } }
// { dg-options "" }

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
#embed __FILE__ limit (264)
  };
  foo (s);
}
