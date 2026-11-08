/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int cnt;
__attribute__((noipa)) void hit(void) { cnt++; }

__attribute__((noipa)) void f(int x) {
  if (((__INT64_TYPE__)(x & -4) & (__INT64_TYPE__)0xFFFFFFFF00000000ULL) == 0 && (x & 1) == 1)
    hit();
}

#if __SIZEOF_LONG__ >= 8
__attribute__((noipa)) void f_long(int x) {
  if (((long)(x & -4) & 0xFFFFFFFF00000000L) == 0 && (x & 1) == 1)
    hit();
}
#endif

int main(void) {
  f(-5);
  if (cnt != 0)
    __builtin_abort();
  f(5);
  if (cnt != 1)
    __builtin_abort();
  f(-1);
  if (cnt != 1)
    __builtin_abort();
  f(1);
  if (cnt != 2)
    __builtin_abort();

#if __SIZEOF_LONG__ >= 8
  cnt = 0;
  f_long(-5);
  if (cnt != 0)
    __builtin_abort();
  f_long(5);
  if (cnt != 1)
    __builtin_abort();
#endif

  return 0;
}
