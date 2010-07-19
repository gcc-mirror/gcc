struct S { };

extern void bar(struct S);

void foo (int i)
{
  bar (*(struct S *)&i);
}
