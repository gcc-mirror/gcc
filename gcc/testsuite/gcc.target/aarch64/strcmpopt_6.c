/* When the specified length exceeds one of the arguments of the call to memcmp, 
   the call to memcmp should NOT be inlined.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-stringop-overflow" } */

typedef struct { char s[8]; int x; } S;

__attribute__ ((noinline)) int
f1 (S * s)
{
  int result = 0;
  result += __builtin_memcmp (s->s, "a", 3); 
  return result;
}

__attribute__ ((noinline)) int
f2 (char *p)
{
  int result = 0;
  result += __builtin_memcmp (p, "a", 3); 
  return result;
}

int main (void)
{
  S ss = {{'a','b','c'}, 2};
  char *s = "abcd";

  if (f1 (&ss) < 0 || f2 (s) < 0)
    __builtin_abort ();

  return 0;

}

/* { dg-final { scan-assembler-times "memcmp" 2 } } */
