// { dg-do assemble  }
// Testcase for simple overloading resolution.

void foo (int);
void foo (int, int);

void bar ()
{
  foo (1);
  foo (1, 2);
}
