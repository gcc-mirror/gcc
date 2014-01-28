// PR c++/58701
// { dg-require-effective-target c++11 }
// { dg-final { scan-assembler "7" } }

static union
{
  union
  {
    int i = 7;
  };
};
