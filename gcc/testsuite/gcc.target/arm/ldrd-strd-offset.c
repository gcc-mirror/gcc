/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct
{
  int x;
  int i, j;
} off_struct;

int foo (char *str, int *a, int b, int c)
{
  off_struct *p = (off_struct *)(str + 3);
  b = p->i;
  c = p->j;
  *a = b + c;
  return 0;
}
