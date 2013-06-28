/* Verify that intermediate coverage format can be generated for simple code. */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

class C {
public:
  C()
  {
    i = 0;
  }
  ~C() {}
  void seti (int j)
  {
    if (j > 0)
      i = j;
    else
      i = 0;
  }
private:
  int i;
};

void foo()
{
  C c;
  c.seti (1);
}

int main()
{
  foo();
}

/* { dg-final { run-gcov intermediate { -i -b gcov-8.C } } } */
