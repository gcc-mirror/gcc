/* Verify that the memory form of NDD instructions is guarded by the
   X86_TUNE_ENABLE_NDD_MEM tune properly. The duplicated preferred_for_speed
   attr will be true when partial_reg_stall enabled for qi patterns, and
   override the ENABLE_NDD_MEM set at first, so there should be only one attr
   remaining by combining the two conditions together. */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapxf -march=x86-64 -O2 -mtune-ctrl=partial_reg_stall" } */

unsigned char gc;

unsigned char
add_qi (unsigned char *p, unsigned char b)
{
  unsigned char r = *p + b;
  gc = r;
  return r;
}

unsigned char
and_qi (unsigned char *p, unsigned char b)
{
  unsigned char r = *p & b;
  gc = r;
  return r;
}

unsigned char
xor_qi (unsigned char *p, unsigned char b)
{
  unsigned char r = *p ^ b;
  gc = r;
  return r;
}

unsigned char
not_qi (unsigned char *p)
{
  unsigned char r = ~*p;
  gc = r;
  return r;
}

unsigned char
shl_qi (unsigned char *p, unsigned char c)
{
  unsigned char r = (unsigned char) (*p << c);
  gc = r;
  return r;
}

/* None of the QImode ops above should use the NDD memory-source form
   when partial_reg_stall && !enable_ndd_mem.  */
/* { dg-final { scan-assembler-not {b[ \t]+%[a-z0-9]+, \(%[a-z0-9]+\),} } } */
