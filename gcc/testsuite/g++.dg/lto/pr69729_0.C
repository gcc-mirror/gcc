/* { dg-lto-do link } */
/* { dg-require-effective-target mpx } */
/* { dg-lto-options {{-fcheck-pointer-bounds -mmpx -flto -flto-partition=max}} } */

class cl1
{
 public:
  virtual ~cl1 () { };
};

class cl2
{
 public:
  virtual ~cl2 () { };
};

class cl3 : cl1, cl2
{
};

class cl4 : cl3
{
 public:
  ~cl4 ();
};

cl4::~cl4 ()
{
}

int main (int argc, char **argv)
{
  cl4 c;
  return 0;
}
