/* { dg-do compile } */

void bar(int);
void foo(void)
{
  for (int i = 0; i<1; ++i)
    bar (i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i*i);
}

