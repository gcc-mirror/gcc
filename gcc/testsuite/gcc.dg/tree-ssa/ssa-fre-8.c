/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra -fdump-tree-fre-details" } */
#if (__SIZEOF_INT__ == __SIZEOF_FLOAT__)
typedef int intflt;
#elif (__SIZEOF_LONG__ == __SIZEOF_FLOAT__)
typedef long intflt;
#else
#error Add target support here for type that will union float size
#endif
union U {
  intflt i;
  float f;
};
intflt foo(int i, int b)
{
  union U u;
  if (b)
    {
      i = i << 2;
      u.i = i;
      return u.f;
    }
  else
    {
      i = i << 2;
      u.i = i;
      return u.f;
    }
}

/* { dg-final { scan-tree-dump-times "Replaced u.f with pretmp" 2 "fre" } } */
/* { dg-final { scan-tree-dump-times "Inserted pretmp" 2 "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
