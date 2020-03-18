/* { dg-do run { target lp64 } } */

struct {
    unsigned long x:33;
} s;
typedef __typeof__(s.x + 0) uint33;

int main()
{
  uint33 x;
  __builtin_memset(&x, -1, sizeof x);
  unsigned long u;
  __builtin_memcpy(&u, &x, sizeof u);
  if (u != -1ul)
    __builtin_abort ();
  return 0;
}
