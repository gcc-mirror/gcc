/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* Fails if lshrsi3_zero_extend_3+1 uses a temp reg which has no REG_DEST
   note.  */
unsigned long
sub (long l)
{
  union u {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    struct s { int a : 19; unsigned int b : 13; int x; } s;
#else
    struct s { int x; unsigned int b : 13; int a : 19; } s;
#endif
    long l;
  } u;
  u.l = l;
  return u.s.b;
}
/* { dg-final { scan-assembler "srliw" } } */
