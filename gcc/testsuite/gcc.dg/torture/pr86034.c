/* PR tree-optimization/86034 */
/* Testcase by  Zhendong Su  <su@cs.ucdavis.edu> */

/* { dg-do run } */

typedef int int32_t __attribute__((mode (__SI__)));

struct A
{
  int32_t b;
  int32_t c:24;
  int32_t d:10;
  int32_t e;
} f;

int32_t g; 

void h ()
{
  struct A i = { 0, 0, -1, 0 };
L:
  f = i;
  i.d = 0;
  if (g < 0)
    goto L;
}

int main (void)
{
  h ();
  if (f.e != 0) 
    __builtin_abort ();
  return 0; 
}
