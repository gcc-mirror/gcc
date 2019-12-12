/* { dg-do run } */
/* { dg-options "-O2 -fdump-rtl-expand" } */

typedef struct { char s[8]; int x; } S;
__attribute__ ((noinline)) int
f1 (S * s)
{
  int result = 0;
  result += __builtin_strncmp (s->s, "ab", 2);
  result += __builtin_strncmp (s->s, "abc", 3);
  return result;
}

__attribute__ ((noinline)) int
f2 (char *p)
{
  int result = 0;
  result += __builtin_strncmp (p, "ab", 2);
  result += __builtin_strncmp (p, "abc", 3);
  return result;
}

__attribute__ ((noinline)) int
f3 (S * s)
{
  int result = 0;
  result += __builtin_strcmp (s->s, "a");
  result += __builtin_strcmp (s->s, "ab");
  return result;
}

__attribute__ ((noinline)) int
f4 (char *p)
{
  int result = 0;
  result += __builtin_strcmp (p, "a");
  result += __builtin_strcmp (p, "ab");
  return result;
}

__attribute__ ((noinline)) int
f5 (S * s)
{
  int result = 0;
  result += __builtin_memcmp (s->s, "ab", 2);
  result += __builtin_memcmp (s->s, "abc", 3);
  return result;
}

__attribute__ ((noinline)) int
f6 (char *p)
{
  int result = 0;
  result += __builtin_memcmp (p, "ab", 2);
  result += __builtin_memcmp (p, "abc", 3);
  return result;
}

int main (void)
{
  S ss = {{'a','b','c'}, 2};
  char *s = "abcd";

  if (f1 (&ss) != 0 || f2 (s) != 0)
    __builtin_abort ();

  if (f3 (&ss) <= 0 || f4 (s) <= 0)
    __builtin_abort ();

  if (f5 (&ss) != 0 || f6 (s) != 0)
    __builtin_abort ();

  return 0;

}


/* { dg-final { scan-rtl-dump-times "__builtin_strcmp" 0 "expand" } } */
/* { dg-final { scan-rtl-dump-times "__builtin_strncmp" 0 "expand" } } */
/* { dg-final { scan-rtl-dump-times "__builtin_memcmp" 0 "expand" } } */
