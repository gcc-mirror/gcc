/* Test to make sure memset of small old size works
   correctly. */
#define SIZE 15

void f(char *a) __attribute__((noinline));
void f(char *a)
{
  __builtin_memset (a, 0, SIZE);
}


int main(void)
{
  int i;
  char b[SIZE];
  for(i = 0; i < sizeof(b); i++)
    {
      b[i] = i;
    }
  f(b);
  for(i = 0; i < sizeof(b); i++)
    {
      if (0 != b[i])
	__builtin_abort ();
    }
  return 0;
}
