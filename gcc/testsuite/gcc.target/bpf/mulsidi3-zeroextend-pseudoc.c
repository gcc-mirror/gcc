/* Make sure that we are emitting `wN *= wM' and not `rN *= wM' for a mul32 in
   pseudo-C assembly syntax when emitting assembly for a recognized
   *mulsidi3_zeroextend pattern.  */

/* { dg-do compile } */
/* { dg-options "-O2 -masm=pseudoc" } */

unsigned long foo (unsigned snd_cwnd, unsigned mss_cache)
{
  return snd_cwnd * mss_cache;
}

/* { dg-final { scan-assembler-not {\tr. \*= w.\n} } } */
/* { dg-final { scan-assembler {\tw. \*= w.\n} } } */
