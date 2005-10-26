/* { dg-do run } */
/* { dg-options "-pg" } */

extern void abort(void);

void foo(int i)
{
  void bar(void)
  {
    if (i != 2)
      abort ();
  }

  bar();
}

int main(void)
{
  foo (2);
  return 0;
}
