/* PR optimization/6703
   Origin: Glen Nakamura <glen@imodulo.com> */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
extern void exit (int);

void foo (int *x, int y)
{
  __builtin_memset (x, 0, y);
}
  
int main ()
{
  int x[2] = { 0x5a5a5a5a, 0x5a5a5a5a };
    
  if (x[1] != 0x5a5a5a5a)
    abort ();
  foo (x, sizeof (int) + 1);
  if (x[1] == 0x5a5a5a5a)
    abort ();
  exit (0);
}
