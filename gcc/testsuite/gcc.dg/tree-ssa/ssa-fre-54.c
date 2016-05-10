/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O -fdump-tree-fre1 -fdump-tree-dse1" } */

extern void abort (void);

union U { int i; char c[4]; short s[2]; };

char __attribute__((noinline,noclone)) foo(int i)
{
  union U u;
  u.i = i;
  /* This should be equivalent to (char) i.  */
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  return u.c[0];
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  return u.c[3];
#else
  return 0x04;
#endif
}

short __attribute__((noinline,noclone)) baz(int i)
{
  union U u;
  u.i = i;
  /* This should be equivalent to (char) i.  */
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  return u.s[0];
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  return u.s[1];
#else
  return 0x0304;
#endif
}

char __attribute__((noinline,noclone)) bar(int j)
{
  union U u;
  u.i = j;
  /* This gets simplified to a BIT_FIELD_REF.  */
  return u.c[2];
}

int main()
{
  if (foo (0x01020304) != 0x04)
    abort ();
  if (baz (0x01020304) != 0x0304)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "\\(char\\) i_" "fre1" } } */
/* { dg-final { scan-tree-dump "\\(short int\\) i_" "fre1" } } */
/* { dg-final { scan-tree-dump-not "u.i =" "dse1" } } */
