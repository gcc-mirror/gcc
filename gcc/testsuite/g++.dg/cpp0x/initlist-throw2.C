// PR c++/85363
// { dg-do run { target c++11 } }

int
init (int f)
{
  throw f;
}

struct X {
  X () : n {init (42)} {}
  int n;
};

struct P {
  struct R {
    struct Q {
      X x = {};
    } q;
  } r;
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
