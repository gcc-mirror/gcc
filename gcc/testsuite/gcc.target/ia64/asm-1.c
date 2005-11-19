/* { dg-do run } */
/* { dg-options } */

extern void abort (void);

/* Test that "=S" properly avoids the post-increment on the memory address.  */

static void foo(int *x)
{
  long i;
  for (i = 0; i < 100; ++i)
    __asm__("st4 %0 = r0" : "=S"(x[i]));
}

int main()
{
  int array[100];
  long i;

  for (i = 0; i < 100; ++i)
    array[i] = -1;

  foo(array);

  for (i = 0; i < 100; ++i)
    if (array[i])
      abort ();
  return 0;
}
