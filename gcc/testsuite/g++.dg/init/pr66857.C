// PR c++/66857
// { dg-do run }

const int i = 0;

struct Test
{
  Test (const int &rhs)
  {
    if (&rhs != &i)
      __builtin_abort ();
  }
};

int
main (void)
{
  Test test = i;
}
