/* PR rtl-optimization/88904 */

volatile int v;

__attribute__((noipa)) void
bar (const char *x, const char *y, int z)
{
  if (!v)
    __builtin_abort ();
  asm volatile ("" : "+g" (x));
  asm volatile ("" : "+g" (y));
  asm volatile ("" : "+g" (z));
}

#define my_assert(e) ((e) ? (void) 0 : bar (#e, __FILE__, __LINE__))

typedef struct {
  unsigned M1;
  unsigned M2 : 1;
  int : 0;
  unsigned M3 : 1;
} S;

S
foo ()
{
  S result = {0, 0, 1};
  return result;
}

int
main ()
{
  S ret = foo ();
  my_assert (ret.M2 == 0);
  my_assert (ret.M3 == 1);
  return 0;
}
