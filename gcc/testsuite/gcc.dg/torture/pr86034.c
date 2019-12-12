/* PR tree-optimization/86034 */
/* Testcase by  Zhendong Su  <su@cs.ucdavis.edu> */

/* { dg-do run } */

struct A
{
  int b;
  __INT32_TYPE__ c:24;
  int d:10;
  int e;
} f;

int g; 

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
