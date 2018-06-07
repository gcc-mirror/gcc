// PR c++/85363
// { dg-do run { target c++11 } }

int
init (int f)
{
  throw f;
}

struct X {
  X (int f) : n {init (f)} {}
  int n;
};

struct P {
  X x{20};
};

int
main ()
{
  try {
    P p {};
  }
  catch (int n) {
    return 0;
  }
  return 1;
}
