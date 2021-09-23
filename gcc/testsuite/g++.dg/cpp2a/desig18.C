// PR c++/100489
// { dg-options "" }

union U
{
  union
  {
    unsigned char a;
  };

  unsigned char b[1];
};

void f(unsigned char a)
{
  union U u = { .a = a };
}
