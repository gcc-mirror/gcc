// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

int i = 1;

template <class T> void test()
{
  goto lab;
 lab: 
  --i;
}

int main ()
{
  test<int>();
  return i;
}
