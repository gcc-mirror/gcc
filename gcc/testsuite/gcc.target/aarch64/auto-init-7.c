/* Verify zero initialization for array, union, and structure type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-rtl-expand -fno-stack-protector" } */

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

/* { dg-final { scan-rtl-dump-times "const_int 0" 8 "expand" } } */
