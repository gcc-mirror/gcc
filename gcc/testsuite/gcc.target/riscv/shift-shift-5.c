/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2" } */

/* Fails if lshrsi3_zero_extend_3+1 uses a temp reg which has no REG_DEST
   note.  */
unsigned long
sub (long l)
{
  union u {
    struct s { int a : 19; unsigned int b : 13; int x; } s;
    long l;
  } u;
  u.l = l;
  return u.s.b;
}
/* { dg-final { scan-assembler "srliw" } } */
