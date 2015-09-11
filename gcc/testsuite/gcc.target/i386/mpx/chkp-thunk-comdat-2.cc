/* { dg-do compile } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx" } */

class c1
{
public:
  virtual int test1 (const char *);
};

class c2
{
public:
  int test2 (const char *);
};

int
c1::test1 (const char *)
{
  return 0;
}

int
c2::test2 (const char *)
{
  return 0;
}
