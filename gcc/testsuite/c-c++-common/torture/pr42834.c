/* { dg-do run } */

void __attribute__((noinline,noclone))
foo(int *p, float *q) { __asm__ volatile ("" : : : "memory"); }

int main()
{
  if (sizeof (int) == sizeof (float))
    {
      int i;
      float f;
      int *p;
      /* Prevent i and f from being rewritten into SSA form.  */
      foo (&i, &f);
      i = 0;
      f = 1.0;
      p = (int *)&f;
      __builtin_memcpy (&i, p, 4);
      if (*(float *)&i != 1.0)
	__builtin_abort ();
    }
  return 0;
}
