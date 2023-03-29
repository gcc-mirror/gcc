/* { dg-do compile } */
/* { dg-options "-Werror=uninitialized -ftrivial-auto-var-init=zero"  } */
int t();
void f(int);

void j()
{
  const int& e = t();
  f(e);
}
