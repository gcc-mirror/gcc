extern void abort (void);
extern void exit (int);

typedef int  __v2hi __attribute ((vector_size(4)));
typedef __v2hi fract2x16;
typedef short fract16;

int main ()
{
  fract2x16 a, b, c, d;
  fract16 t1, t2;
  a = __builtin_bfin_compose_2x16 (0x3000, 0x2000);
  b = __builtin_bfin_compose_2x16 (0x7000, 0x5000);
  c = __builtin_bfin_compose_2x16 (0x7000, 0xc000);

  d = __builtin_bfin_add_fr2x16 (a, b);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if (t1 != 0x7000 || t2 != 0x7fff)
    abort ();

  d = __builtin_bfin_sub_fr2x16 (a, b);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if (t1 != -0x3000 || t2 != -0x4000)
    abort ();

  d = __builtin_bfin_negate_fr2x16 (c);
  t1 = __builtin_bfin_extract_lo (d);
  t2 = __builtin_bfin_extract_hi (d);
  if (t1 != 0x4000 || t2 != -0x7000)
    abort ();
  
  if (__builtin_bfin_add_fr1x32 (0x7fffffff, 1) != 0x7fffffff)
    abort ();

  if (__builtin_bfin_add_fr1x32 (0x80000000, -1) != 0x80000000)
    abort ();
    
  if (__builtin_bfin_add_fr1x32 (0x80000001, -1) != 0x80000000)
    abort ();
    
  if (__builtin_bfin_add_fr1x32 (0xFEDCBA98, 0x11111111) != 0x0FEDCBA9)
    abort ();
    
  exit (0);
}

