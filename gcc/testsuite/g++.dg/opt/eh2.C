// PR 6764
// { dg-do run }
// { dg-options "-O -fomit-frame-pointer" }

extern "C" void abort ();

class test
{
 public:
  test * const me;
  test () : me(this) { }
  ~test () { if (me != this) abort (); }
};

void x1 ()
{
  test w1;
  throw 1;
}

void x2 ()
{
  test w2;
  x1 ();
}

int main (void)
{
  try {
    x2 ();
  } catch (...) {
  }
  return 0;
}
