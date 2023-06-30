#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define REVERSE_SSO __attribute__((scalar_storage_order("big-endian")));
#else
#define REVERSE_SSO __attribute__((scalar_storage_order("little-endian")));
#endif

struct S {
  int i : 24;
  char c1 : 1;
  char c2 : 1;
  char c3 : 1;
  char c4 : 1;
  char c5 : 1;
  char c6 : 1;
  char c7 : 1;
  char c8 : 1;
} REVERSE_SSO;

int main (void)
{
  struct S s0 = { 1193046, 1, 1, 1, 1, 1, 1, 1, 1 };
  char *p = (char *) &s0;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  if (*p != 18)
    __builtin_abort ();
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  if (*p != 86)
    __builtin_abort ();
#endif

  return 0;
}
