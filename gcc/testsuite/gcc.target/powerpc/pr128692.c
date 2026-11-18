/* { dg-do assemble { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mcpu=power9 -mtune=power10 -fno-pic" } */

/* PR target/128692.  With -mtune=power10 the *largetocp10 patterns in
   rs6000.md emit two instructions but used to have the default length of
   4.  In a sufficiently large function this made shorten_branches
   underestimate the distance to a label and emit an out-of-range short
   branch.  */

#define ITEM_(n) extern volatile long v##n; if (c == n) goto far; s += v##n;
#define ITEM(n) ITEM_(n)
#define R1(M) M(__COUNTER__)
#define R2(M) R1(M) R1(M)
#define R4(M) R2(M) R2(M)
#define R8(M) R4(M) R4(M)
#define R16(M) R8(M) R8(M)
#define R32(M) R16(M) R16(M)
#define R64(M) R32(M) R32(M)
#define R128(M) R64(M) R64(M)
#define R256(M) R128(M) R128(M)
#define R512(M) R256(M) R256(M)
#define R1024(M) R512(M) R512(M)

long f (long c)
{
  long s = 0;
  R1024(ITEM)
  R128(ITEM)
  R32(ITEM)
  R16(ITEM)
far:
  s += 1;
  return s;
}
