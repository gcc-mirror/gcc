#if (__SIZEOF_INT__ == 4)
typedef int int32;
#elif (__SIZEOF_LONG__ == 4)
typedef long int32;
#else
#error Add target support for int32
#endif
static int32 ref(void)
{
  union {
    char c[5];
    int32 i;
  } u;

  __builtin_memset (&u, 0, sizeof(u));
  u.c[0] = 1;
  u.c[1] = 2;
  u.c[2] = 3;
  u.c[3] = 4;

  return u.i;
}

int main()
{
  int32 b = ref();
  if (b != 0x01020304
      && b != 0x04030201)
    abort ();
  return 0;
}
