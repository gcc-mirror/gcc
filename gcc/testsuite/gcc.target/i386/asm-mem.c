/* { dg-do run } */
/* { dg-options "-O3" } */

/* Check that "m" array references are effective in preventing the
   array initialization from wandering past a use in the asm, and
   that the casts remain supported.  */

static int
f1 (const char *p)
{
  int count;

  __asm__ ("repne scasb"
	   : "=c" (count), "+D" (p)
	   : "m" (*(const char (*)[]) p), "0" (-1), "a" (0));
  return -2 - count;
}

static int
f2 (const char *p)
{
  int count;

  __asm__ ("repne scasb"
	   : "=c" (count), "+D" (p)
	   : "m" (*(const char (*)[48]) p), "0" (-1), "a" (0));
  return -2 - count;
}

static int
f3 (int n, const char *p)
{
  int count;

  __asm__ ("repne scasb"
	   : "=c" (count), "+D" (p)
	   : "m" (*(const char (*)[n]) p), "0" (-1), "a" (0));
  return -2 - count;
}

int
main ()
{
  int a;
  char buff[48] = "hello world";
  buff[4] = 0;
  a = f1 (buff);
  if (a != 4)
    __builtin_abort ();
  buff[4] = 'o';
  a = f2 (buff);
  if (a != 11)
    __builtin_abort ();
  buff[4] = 0;
  a = f3 (48, buff);
  if (a != 4)
    __builtin_abort ();
  return 0;
}
