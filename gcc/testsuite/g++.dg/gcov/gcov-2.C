/* Verify line coverage counts for simple member functions. */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

class C {
public:
  C()
  {
    i = 0;				/* count(1) */
  }
  ~C() {}
  void seti (int j)
  {
    i = j;				/* count(1) */
  }
private:
  int i;
};

void foo()
{
  C c;					/* count(1) */
  c.seti (1);				/* count(1) */
}

int main()
{
  foo();				/* count(1) */
}

/* { dg-final { run-gcov gcov-2.C } } */
