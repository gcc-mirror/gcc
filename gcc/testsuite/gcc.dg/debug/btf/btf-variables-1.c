/* BTF generation for variables. */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* We expect 6 variables */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*btv_info" 6 } } */

unsigned int x1;

struct st
{
  int a;
  int b;
};

union {
  long int value;
  struct st * pointer;
} bar;

enum
{
  FOO = 0,
  BAR = 2,
  BAZ,
} lala;

int arr[10][20];

unsigned long * plong;

struct st st_inst;
