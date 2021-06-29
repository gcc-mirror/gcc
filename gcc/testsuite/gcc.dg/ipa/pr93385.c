/* { dg-do run } */
/* { dg-options "-O2 -fno-dce -fno-ipa-cp -fno-tree-dce" } */

char a, b;

#ifdef __SIZEOF_INT128__
#define T unsigned __int128
#else
#define T unsigned
#endif

static inline int
c (T d)
{
  char e = 0;
  d %= (unsigned) d;
  e -= 0;
  __builtin_strncpy (&a, &e, 1);
  return e + b;
}

int
main (void)
{
  c (~0);
  return 0;
}
