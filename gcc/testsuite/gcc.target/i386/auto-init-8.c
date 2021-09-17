/* Verify pattern initialization for array, union, and structure type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern -fdump-rtl-expand -march=x86-64 -mtune=generic -msse" } */

struct S
{
  int f1;
  float f2;
  char f3[20];
};

union U
{
  char u1[5];
  int u2;
  float u3; 
};

double result;

double foo()
{
  int temp1[3];
  double temp2[3];
  struct S temp3;
  union U temp4;
  
  result = temp1[2] + temp2[1] + temp3.f2 + temp4.u3;
  return result;
}

/* { dg-final { scan-rtl-dump-times "0xfffffffffefefefe" 1 "expand" } } */
/* { dg-final { scan-rtl-dump-times "\\\[0xfefefefefefefefe\\\]" 2 "expand" } } */
/* { dg-final { scan-rtl-dump-times "0xfffffffffffffffe\\\]\\\) repeated x16" 2 "expand" } } */

