// Testcase for simple overloading resolution.
// Build don't link:

void foo (int);
void foo (int, int);

void bar ()
{
  foo (1);
  foo (1, 2);
}
