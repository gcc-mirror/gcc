/* { dg-do run } */

char a[128];

void __attribute__((noipa))
foo ()
{
  for (unsigned i = 27; i >= 5; --i)
    a[i] = a[i-5];
}

int main()
{
  __builtin_memcpy (a, "Hello World", sizeof ("Hello World"));
  foo ();
  if (__builtin_memcmp (a + 5, "Hello World", sizeof ("Hello World")) != 0)
    __builtin_abort ();
  return 0;
}
