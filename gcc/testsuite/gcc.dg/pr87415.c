/* { dg-do run } */
/* { dg-options "-O2" } */

struct A
{
  int b:1;
};

int d;

int main ()
{
  struct A e = { 0 };
  if (!d)
    e.b = -1;
  if (!e.b)
    __builtin_abort ();

  return 0;
}
