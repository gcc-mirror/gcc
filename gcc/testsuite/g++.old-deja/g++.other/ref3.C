// Origin: Mark Mitchell <mark@codesourcery.com>

struct B1
{
  int i;
};

struct B2
{
  int j;
};

struct D: public B1, B2 
{
};

bool f (B2& b)
{
  return b.j == 7;
}

int main ()
{
  D d;
  d.i = 2;
  d.j = 7;
  if (!f (d))
    return 1;
}

