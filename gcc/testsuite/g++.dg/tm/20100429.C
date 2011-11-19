// { dg-do compile }
// { dg-options "-fgnu-tm" }

int foo(int a);
int foo(float a);
int
bar(int a)
{
  int r;
  __transaction_atomic
    {
      r = foo(a); // { dg-error "unsafe function call 'int foo\\(int\\)'" }
    }
  return r;
}
