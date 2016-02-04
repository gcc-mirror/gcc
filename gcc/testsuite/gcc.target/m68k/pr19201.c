/* { dg-options "-w -O2 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler-not "%a.,%\[ad\]..l" } } */

struct X { 
  char *a; 
  /* other members */ 
  int b; 
}; 
 
void f (struct X *x) 
{ 
  x->a[x->b] = 0; 
}
