/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zicond -mabi=lp64d" { target rv64 } } */
/* { dg-options "-O2 -march=rv64gc_zicond -mabi=lp64d" { target rv32 } } */

/* (p != 0) & (s CC t) is p ? (s CC t) : 0, i.e. an sCC feeding czero.eqz.  */

/* lt: register operand.  */
long
f_lt (const void *p, long s, long t)
{
  return (p != 0) & (s < t);
}

/* ltu: register operand.  */
long
f_ltu (const void *p, unsigned long s, unsigned long t)
{
  return (p != 0) & (s < t);
}

/* le: s < 4 is canonicalized to s <= 3.  */
long
f_le (const void *p, long s)
{
  return (p != 0) & (s < 4);
}

/* leu: s < 4 is canonicalized to s <=u 3.  */
long
f_leu (const void *p, unsigned long s)
{
  return (p != 0) & (s < 4);
}

/* { dg-final { scan-assembler-times "czero.eqz" 4 } } */
/* { dg-final { scan-assembler-not "snez" } } */

