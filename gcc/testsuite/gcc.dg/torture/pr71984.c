/* { dg-do run { target lp64 } } */
/* { dg-additional-options "-w -Wno-psabi" } */

typedef unsigned char v64u8 __attribute__((vector_size(64)));
typedef unsigned long v64u64 __attribute__((vector_size(64)));
typedef unsigned char u8;

static u8 __attribute__ ((noinline, noclone))
foo (v64u64 v64u64_0)
{
  return ((v64u8)(v64u64){0, v64u64_0[0]})[13];
}

int
main ()
{
  u8 x = foo((v64u64){0x0706050403020100UL});
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  if (x != 5)
    __builtin_abort ();
#endif
  return 0;
}
