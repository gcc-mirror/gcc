// { dg-do run  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct A
{
  union 
  {
    int i;
  };
  int j;

  A ();
};

A::A ()
  : i (1), j (i = 0)
{
}

int main ()
{
  A a;
  return a.i;
}
