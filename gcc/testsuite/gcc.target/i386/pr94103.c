/* { dg-do run { target lp64 } } */
/* { dg-options "-O3" } */

int main()
{
  long double x;
  unsigned long u[2] = {0xEEEEEEEEEEEEEEEEUL, 0xEEEEEEEEEEEEEEEEUL};
  __builtin_memcpy(&x, &u, sizeof x);
  __builtin_memcpy(&u, &x, sizeof u);
  ++*(unsigned char *)&x;
  (void)-x;
  __builtin_memcpy(&u, &x, sizeof u);
  if (u[1] != 0xEEEEEEEEEEEEEEEEUL
      || u[0] != 0xEEEEEEEEEEEEEEEFUL)
    __builtin_abort ();
  return 0;
}
