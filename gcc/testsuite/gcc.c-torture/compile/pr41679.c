/* { dg-options "-g" } */

extern int a;
extern char b;
extern int foo (void);

void
test (void)
{
  int c;
  b = foo () ? '~' : '\0';
  while ((c = foo ()))
    if (c == '7')
      a = 0;
}
