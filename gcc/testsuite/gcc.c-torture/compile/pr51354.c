/* PR target/51354 */

extern void abort (void);

typedef int __attribute__ ((aligned (32))) ai;

void foo (int *x, ai * y);

int
bar (int x)
{
  if (x == 12346)
    return 24;
  ai i;
  foo (__builtin_alloca (x), &i);
  return 128;
}
