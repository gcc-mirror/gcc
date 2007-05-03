extern void abort (void);
extern void exit (int);

typedef short  __v2hi __attribute ((vector_size(4)));
typedef __v2hi fract2x16;
typedef short fract16;

int main ()
{
  fract2x16 a, b, c, d;
  fract16 t1, t2;
  a = __builtin_bfin_compose_2x16 (0xe005, 0x1000);
  b = __builtin_bfin_compose_2x16 (0x7000, 0x5000);
  c = __builtin_bfin_compose_2x16 (0x7000, 0xc000);

  d = __builtin_bfin_shl_fr2x16 (c, 2);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if ((unsigned short)t1 != 0x8000 || t2 != 0x7fff)
    abort ();

  d = __builtin_bfin_shl_fr2x16 (c, -2);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if ((unsigned short)t1 != 0xf000 || t2 != 0x1c00)
    abort ();

  d = __builtin_bfin_shl_fr2x16 (a, 2);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if (t1 != 0x4000 || (unsigned short)t2 != 0x8014)
    abort ();

  d = __builtin_bfin_shl_fr2x16 (c, -4);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if ((unsigned short)t1 != 0xfc00 || t2 != 0x0700)
    abort ();

  d = __builtin_bfin_shl_fr2x16 (c, 2);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if ((unsigned short)t1 != 0x8000 || t2 != 0x7fff)
    abort ();

  d = __builtin_bfin_shl_fr2x16 (a, -2);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if (t1 != 0x0400 || (unsigned short)t2 != 0xf801)
    abort ();

  /* lsh */
  d = __builtin_bfin_lshl_fr2x16 (c, -4);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if (t1 != 0x0c00 || t2 != 0x0700)
    abort ();

  d = __builtin_bfin_lshl_fr2x16 (c, 2);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if (t1 != 0x0000 || t2 != -0x4000)
    abort ();

  d = __builtin_bfin_lshl_fr2x16 (a, -2);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if (t1 != 0x0400 || (unsigned short)t2 != 0x3801)
    abort ();

  exit (0);
}

