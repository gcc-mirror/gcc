void bar (struct S *);
void foo (void *x)
{
  bar ((struct S *) x);
}
