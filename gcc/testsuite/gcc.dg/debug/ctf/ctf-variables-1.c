/* CTF generation for global variables.

   In this testcase, 7 records in the variable info section are expected.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "ctv_name" 7 } } */

float var1;
double var2;
long double var3;

char ascii = 'a';

int a = 33;
int a1[2] = {22, 33};

struct d
{
  int d1;
  int d2;
};

struct d d_instance;
