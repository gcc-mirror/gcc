/* { dg-do run } */
/* { dg-require-alias "" } */

extern void abort (void);

int a = 1, c = 1;
extern int b __attribute__((alias("a")));
extern int d __attribute__((alias("c")));
int main(int argc)
{
  int *p, *q;
  if (argc >= 0)
    p = &c, q = &d;
  else
    p = &b, q = &d;
  *p = 1;
  *q = 2;
  if (*p == 1)
    abort();
  return 0;
}
