// { dg-do run { target fpic } }
// { dg-options "-O3 -fpic" }

struct CBase {
  virtual void BaseFunc () {}
};

struct MMixin {
  virtual void * MixinFunc (int, int) = 0;
};

struct CExample: CBase, public MMixin
{
  void *MixinFunc (int arg, int arg2)
  {
    return this;
  }
};

void *test (MMixin & anExample)
{
  return anExample.MixinFunc (0, 0);
}

int main ()
{
  CExample c;
  return (test (c) != &c);
}
