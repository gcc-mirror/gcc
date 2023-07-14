signed char f() __attribute__((__noipa__));
signed char f() { return 0; }
int main()
{
  int g = f() - 1;
  int e = g < 0 ? 1 : ((g >> (8-2))!=0);
  asm("":"+r"(e));
  if (e != 1)
    __builtin_abort();
}
