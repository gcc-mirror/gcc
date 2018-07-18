/* PR rtl-optimization/84071 */
/* Reported by Wilco <wilco@gcc.gnu.org> */

extern void abort (void);

typedef union 
{
  signed short ss;
  unsigned short us;
  int x;
} U;

int f(int x, int y, int z, int a, U u) __attribute__((noclone, noinline));

int f(int x, int y, int z, int a, U u)
{
  return (u.ss <= 0) + u.us;
}

int main (void)
{
  U u = { .ss = -1 };

  if (f (0, 0, 0, 0, u) != (1 << sizeof (short) * 8))
    abort ();

  return 0;
}
