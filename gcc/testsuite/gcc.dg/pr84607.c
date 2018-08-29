/* { dg-do run } */

extern void exit(int);
extern void abort(void);
int a[10];
int foo()
{
  exit (0);
  return 0;
}
int main()
{
  if (&a[foo()])
    abort ();
  return 0;
}
