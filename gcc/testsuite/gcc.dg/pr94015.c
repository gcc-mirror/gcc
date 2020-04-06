/* PR tree-optimization/94015 */
/* { dg-do run } */
/* { dg-options "-O2" } */

char buf[10] = "AAAAAAAAA";

__attribute__((noipa)) char *
alloc (void)
{
  return buf;
}

__attribute__((noipa)) void
f1 (void)
{
  char *s = alloc ();
  *(char **)s = "1234567";
  s[7] = '\0';
}

__attribute__((noipa)) void
f2 (void)
{
  char *s = alloc ();
  *(char **)s = "123456";
  s[6] = '\0';
}

__attribute__((noipa)) void
f3 (void)
{
  char *s = alloc ();
  *(char **)s = "12345";
  s[5] = '\0';
}

__attribute__((noipa)) void
f4 (void)
{
  char *s = alloc ();
  *(char **)s = "1234";
  s[4] = '\0';
}

__attribute__((noipa)) void
f5 (void)
{
  char *s = alloc ();
  *(char **)s = "123";
  s[3] = '\0';
}

__attribute__((noipa)) void
f6 (void)
{
  char *s = alloc ();
  *(char **)s = "12";
  s[2] = '\0';
}

__attribute__((noipa)) void
f7 (void)
{
  char *s = alloc ();
  *(char **)s = "1";
  s[1] = '\0';
}

__attribute__((noipa)) void
f8 (void)
{
  char *s = alloc ();
  *(char **)s = "";
  s[0] = '\0';
}

int
main ()
{
  if (sizeof (char *) > 8)
    return 0;
  f1 ();
  if (buf[7] != 0)
    __builtin_abort ();
  f2 ();
  if (buf[6] != 0)
    __builtin_abort ();
  f3 ();
  if (buf[5] != 0)
    __builtin_abort ();
  f4 ();
  if (buf[4] != 0)
    __builtin_abort ();
  f5 ();
  if (buf[3] != 0)
    __builtin_abort ();
  f6 ();
  if (buf[2] != 0)
    __builtin_abort ();
  f7 ();
  if (buf[1] != 0)
    __builtin_abort ();
  f8 ();
  if (buf[0] != 0)
    __builtin_abort ();
  return 0;
}
