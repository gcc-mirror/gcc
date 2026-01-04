/* { dg-do run } */
/* { dg-options "-O2" } */

int a;
long b;
int *c = &a;
short d(short e, short f) { return e * f; }

__attribute__((noipa))
void foo() {
  *c = d(340, b >= 0) ^ 3;
}

int main()
{
  foo();

  if (a != 343)
    __builtin_abort();
}
