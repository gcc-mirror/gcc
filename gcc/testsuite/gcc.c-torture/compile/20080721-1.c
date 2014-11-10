/* { dg-skip-if "can't read function data" { nvptx-*-* } { "*" } { "" } } */
void foo(void);
void bar(void);

int test(int b)
{
  void *p, **q;
  if (b)
    p = (void *)foo;
  else
    p = (void *)bar;
  q = (void **)p;
  if (*q == (void *)0)
    return 1;
  return 0;
}
