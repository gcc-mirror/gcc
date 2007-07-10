void func(void) __attribute__((aligned(256)));

void func(void) 
{
}

int main()
{
  if (((long)func & 0xFF) != 0)
    abort ();
  if (__alignof__(func) != 256)
    abort ();
  return 0;
}
