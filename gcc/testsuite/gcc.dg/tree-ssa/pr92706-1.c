/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fdump-tree-esra-details" } */

struct S { int i[4]; } __attribute__((aligned(128)));
typedef __int128_t my_int128 __attribute__((may_alias));
__int128_t load (void *p)
{
  struct S v;
  __builtin_memcpy (&v, p, sizeof (struct S));
  struct S u;
  u = v;
  struct S w;
  w = u;
  return *(my_int128 *)&w;
}

/* { dg-final { scan-tree-dump-not "Created a replacement for u offset: \[^0\]" "esra" } } */
