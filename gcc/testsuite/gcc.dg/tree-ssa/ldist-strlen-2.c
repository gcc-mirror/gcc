/* { dg-do run } */
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-details" } */
/* { dg-final { scan-tree-dump-times "generated strlenQI\n" 3 "ldist" } } */

#include <assert.h>

typedef __SIZE_TYPE__ size_t;
extern void* malloc (size_t);
extern void* memset (void*, int, size_t);

__attribute__((noinline))
int test_pos (char *s)
{
  int i;
  for (i=42; s[i]; ++i);
  return i;
}

__attribute__((noinline))
int test_neg (char *s)
{
  int i;
  for (i=-42; s[i]; ++i);
  return i;
}

__attribute__((noinline))
int test_including_null_char (char *s)
{
  int i;
  for (i=1; s[i-1]; ++i);
  return i;
}

int main(void)
{
  void *p = malloc (1024);
  assert (p);
  memset (p, 0xf, 1024);
  char *s = (char *)p + 100;

  s[42+13] = 0;
  assert (test_pos (s) == 42+13);
  s[42+13] = 0xf;

  s[13] = 0;
  assert (test_neg (s) == 13);
  s[13] = 0xf;

  s[-13] = 0;
  assert (test_neg (s) == -13);
  s[-13] = 0xf;

  s[13] = 0;
  assert (test_including_null_char (s) == 13+1);

  return 0;
}
