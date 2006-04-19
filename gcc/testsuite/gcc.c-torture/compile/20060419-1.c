/* This failed because if conversion didn't handle insv patterns properly.  */

union y
{
  int a;
  unsigned short b;
};

extern void bar (unsigned short u, union y v);

void
foo (int check)
{
  union y x;

  if (check != 0)
    x.b = 1;
  else
    x.b = 2;
  bar (x.b, x);
}
