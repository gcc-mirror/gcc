struct S
{
  int i;
};

extern void bar (struct S);

void
foo (void)
{
  int i = 0;
  bar (*(struct S *) &i);
}
