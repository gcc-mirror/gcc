/* { dg-do run } */
/* { dg-options "-O" } */

__attribute__((noipa))
int f(void)
{
  int o = 0;
  for (int i = 0; i < 3; i++)
    {
      register int a asm("eax");
      a = 1;
      asm("add %1, %0" : "+r"(o) : "r"(a));
      asm("xor %%eax, %%eax" ::: "eax");
    }
  return o;
}

int main()
{
  if (f() != 3)
    __builtin_abort();
  return 0;
}
