/* { dg-do run } */
/* { dg-options "-O3"  } */

struct A {
  void *p;
  A (void *q) : p (q) {}
  A (const A &) : p () {}
};

struct CBase {
  virtual void BaseFunc () {}
};

struct MMixin {
  virtual A MixinFunc (int, A) = 0;
};

struct CExample: CBase, public MMixin
{
  A MixinFunc (int arg, A arg2)
  {
    if (arg != 1 || arg2.p)
      return 0;
    return this;
  }
};

void *test (MMixin & anExample)
{
  return anExample.MixinFunc (1, (0)).p;
}

int main ()
{
  CExample c;
  test (c);
  return 0;
}
