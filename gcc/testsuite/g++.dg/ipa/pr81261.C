/* { dg-do compile } */
/* { dg-options "-O1 -fno-guess-branch-probability"  } */

struct CBase {
  virtual void BaseFunc () {}
};

struct MMixin {
  virtual void * MixinFunc (int, void *) = 0;
};

struct CExample: CBase, public MMixin
{
  void *MixinFunc (int arg, void *arg2)
  {
    if (arg != 1 || arg2)
      return 0;
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
