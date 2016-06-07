/* { dg-do run } */

struct S1
{
  int f1:1;
};

volatile struct S1 b = { 0 };

int
main ()
{
  char c = b.f1;
  b.f1 = 1; 

  if (b.f1 > -1 || c)
    __builtin_abort (); 

  return 0; 
}
