/* { dg-do run } */
/* { dg-additional-options "-Wno-psabi" } */

#define vect8 __attribute__((vector_size(8)))

vect8 int __attribute__((noipa))
f(int a)
{
  int b;
  vect8 int t={1,1};
  if(a) return t;
  return (vect8 int){0, b};
}

int main ()
{
  if (f(0)[0] != 0)
    __builtin_abort ();
  return 0;
}
