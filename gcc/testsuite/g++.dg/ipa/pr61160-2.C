/* { dg-do run } */
/* { dg-options "-O3 --param ipa-cp-eval-threshold=1"  } */

extern "C" void abort (void);

struct CBase {
  virtual void BaseFunc () {}
};

struct MMixin {
  virtual void * MixinFunc (int, void *) = 0;
};

struct CExample: CBase, public MMixin
{
  int stuff, magic, more_stuff;

  CExample ()
  {
    stuff = 0;
    magic = 0xbeef;
    more_stuff = 0;
  }
  void *MixinFunc (int arg, void *arg2)
  {
    if (arg != 1 || arg2)
      return 0;
    if (magic != 0xbeef)
      abort();
    return this;
  }
};

void *test (MMixin & anExample)
{
  return anExample.MixinFunc (1, 0);
}

int main ()
{
  CExample c;
  return (test (c) != &c);
}
