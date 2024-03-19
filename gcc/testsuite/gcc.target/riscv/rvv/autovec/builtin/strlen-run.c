/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-O3 -minline-strlen" } */

int
__attribute__ ((noipa))
foo (const char *s)
{
  return __builtin_strlen (s);
}

int
__attribute__ ((noipa))
foo2 (const char *s)
{
  int n = 0;
  while (*s++ != '\0')
    {
      asm volatile ("");
      n++;
    }
  return n;
}

#define SZ 10

int main ()
{
  const char *s[SZ]
    = {"",  "asdf", "0", "\0", "!@#$%***m1123fdnmoi43",
       "a", "z",    "1", "9",  "12345678901234567889012345678901234567890"};

  for (int i = 0; i < SZ; i++)
    {
      if (foo (s[i]) != foo2 (s[i]))
        __builtin_abort ();
    }
}
