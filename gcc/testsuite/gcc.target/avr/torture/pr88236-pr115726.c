/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options "-std=gnu99" } */

const __flash char fvals8[] = { 1, 2, 3 };
char rvals8[] = { 0, 2, 4 };

const __flash int fvals16[] = { 1, 2, 3 };
int rvals16[] = { 0, 2, 4 };

__attribute__((noipa))
char xload8_r30 (const __memx char *pc)
{
  register char c __asm ("r30");
  c = *pc;
  __asm (";;" : "+r" (c));
  return c;
}

__attribute__((noipa))
int xload16_r30 (const __memx int *pc)
{
  register int c __asm ("r30");
  c = *pc;
  __asm (";;" : "+r" (c));
  return c;
}

__attribute__((noipa))
char xload8_r22 (const __memx char *pc)
{
  register char c __asm ("r22");
  c = *pc;
  __asm (";;" : "+r" (c));
  return c;
}

__attribute__((noipa))
int xload16_r22 (const __memx int *pc)
{
  register int c __asm ("r22");
  c = *pc;
  __asm (";;" : "+r" (c));
  return c;
}

__attribute__((noipa))
int xload16_r20 (const __memx int *pc)
{
  register int c __asm ("r20");
  c = *pc;
  __asm (";;" : "+r" (c));
  return c;
}

void test8 (void)
{
  char c;
  for (int i = 0; i < 3; ++i)
    {
      c = xload8_r30 (fvals8 + i);
      if (c != 1 + i)
	__builtin_exit (__LINE__);

      c = xload8_r22 (fvals8 + i);
      if (c != 1 + i)
	__builtin_exit (__LINE__);

      c = xload8_r30 (rvals8 + i);
      if (c != 2 * i)
	__builtin_exit (__LINE__);

      c = xload8_r22 (rvals8 + i);
      if (c != 2 * i)
	__builtin_exit (__LINE__);
    }
}

void test16 (void)
{
  int c;
  for (int i = 0; i < 3; ++i)
    {
      c = xload16_r30 (fvals16 + i);
      if (c != 1 + i)
	__builtin_exit (__LINE__);

      c = xload16_r22 (fvals16 + i);
      if (c != 1 + i)
	__builtin_exit (__LINE__);

      c = xload16_r20 (fvals16 + i);
      if (c != 1 + i)
	__builtin_exit (__LINE__);

      c = xload16_r30 (rvals16 + i);
      if (c != 2 * i)
	__builtin_exit (__LINE__);

      c = xload16_r22 (rvals16 + i);
      if (c != 2 * i)
	__builtin_exit (__LINE__);

      c = xload16_r20 (rvals16 + i);
      if (c != 2 * i)
	__builtin_exit (__LINE__);
    }
}

int main (void)
{
  test8();
  test16();

  return 0;
}
