/* { dg-do run } */
/* { dg-options "-O2" } */

int func_pure (void);
void func_other (int);
int global_int;
int func_pure (void) { return global_int; }
void func_other (int a)
{
  global_int = a + 1;
}
int f(void)
{
  int a;
  a = func_pure();
  func_other (a);
  a = func_pure (); // We were removing this function call
  return a;
}
void abort (void);

int main(void)
{
  global_int = 10;
  if (f() != 11)
    abort ();
  return 0;
}
