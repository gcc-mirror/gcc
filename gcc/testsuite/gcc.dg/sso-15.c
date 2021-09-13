/* { dg-do compile } */
/* { dg-options "-O2" } */

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define REV_ENDIANNESS __attribute__((scalar_storage_order("big-endian")))
#else
#define REV_ENDIANNESS __attribute__((scalar_storage_order("little-endian")))
#endif

struct X { int *p; } REV_ENDIANNESS;

struct X x;

struct X __attribute__((noinline)) foo (int *p)
{
  struct X x;
  x.p = p;
  return x;
}

void __attribute((noinline)) bar (void)
{
  *x.p = 1;
}

extern void abort (void);

int main (void)
{
  int i = 0;
  x = foo(&i);
  bar();
  if (i != 1)
    abort ();
  return 0;
}
