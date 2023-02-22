/* { dg-do run } */
/* { dg-options "-O2" } */


#ifdef __XTENSA_CALL0_ABI__
void __xtensa_libgcc_window_spill (void)
{
}
#else
void __xtensa_libgcc_window_spill (void);
#endif

__attribute__((noinline)) void h (void)
{
  __xtensa_libgcc_window_spill ();
}

int f (int u, int v)
{
  int a = u;
  int s;

  __attribute__((noinline,pure)) int nested1 (int b)
  {
      h();
      return a + b;
  }

  __attribute__((noinline,pure)) int nested2 (int b)
  {
      h();
      return a - b;
  }

  s = nested1 (v);
  s += nested2 (v);
  return s;
}

int main (void)
{
  int u = 0x12345678;
  int v = 1;

  return f (u, v) == 2 * u ? 0 : 1;
}
