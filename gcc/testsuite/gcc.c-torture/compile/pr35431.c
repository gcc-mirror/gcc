void bar();

void foo(int i)
{
  __complex__ int k = 0;

  if (i)
    k = 1;

  for (i = 0; i < 1; ++i)
    ;

  if (k)
    bar();
}
