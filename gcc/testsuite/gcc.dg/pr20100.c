/* { dg-do run } */
/* { dg-options "-O2" } */

int func_pure (void) __attribute__ ((pure));
void func_other (int);
int global_int;
void abort ();
void func_other(int a)
{
  if (a != global_int)
   abort ();
  global_int++;
}

int func_pure(void)
{
  return global_int;
}

int
func_loop (int arg)
{
 // global_int ++;
  while (arg--)
      func_other (func_pure ());
}

int main(void)
{
  func_loop(10);
  return 0;
}
