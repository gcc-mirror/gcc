/* PR target/71048: a write-only store to a misaligned volatile object must
   not read the destination back.  The store is decomposed into byte/halfword
   accesses on a strict-alignment target; each unit is fully written, so no
   read-modify-write (and in particular no spurious volatile read) is needed.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-unaligned-access" } */

struct __attribute__((packed)) S { unsigned char pad; volatile unsigned val; };

void
wr (struct S *m, unsigned x)
{
  m->val = x;
}

/* No load of the volatile destination should be emitted (matches ldr, ldrb,
   ldrh).  */
/* { dg-final { scan-assembler-not {\mldr[bhd]?\M} } } */
/* { dg-final { scan-assembler-times {\mstrb\M} 4 } } */
