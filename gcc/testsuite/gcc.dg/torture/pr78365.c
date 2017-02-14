/* { dg-do compile } */

int a, b, c;
char d;
static void fn1 (void *, int);
int fn2 (int);

void fn1 (cc, yh) void *cc;
char yh;
{
  char y;
  a = fn2(c - b + 1);
  for (; y <= yh; y++)
    ;
}

void fn3()
{
    fn1((void *)fn3, 1);
    fn1((void *)fn3, d - 1);
}
