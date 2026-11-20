/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

void tick (void);

void
avg_then_call (unsigned char *dst, const unsigned char *s1,
	       const unsigned char *s2, int w, int h, int ds, int ss1, int ss2)
{
  for (int y = 0; y < h; y++)
    {
      for (int x = 0; x < w; x++)
	dst[x] = (s1[x] + s2[x] + 1) >> 1;
      dst += ds;
      s1 += ss1;
      s2 += ss2;
    }
  tick ();
}

void
call_then_avg (unsigned char *dst, const unsigned char *s1,
	       const unsigned char *s2, int w, int h, int ds, int ss1, int ss2)
{
  tick ();
  for (int y = 0; y < h; y++)
    {
      for (int x = 0; x < w; x++)
	dst[x] = (s1[x] + s2[x] + 1) >> 1;
      dst += ds;
      s1 += ss1;
      s2 += ss2;
    }
}

/* avg_then_call sets VXRM before the first local branch label, so neither
   loop repeats the write.  Allow the prologue and zero-trip guards.  */
/* { dg-final { scan-assembler {avg_then_call:\n(?:(?!\.L[0-9]+:)[^\n]*\n)*?\tcsrwi\tvxrm,0\n} } } */

/* call_then_avg establishes VXRM after tick for the vector average.  */
/* { dg-final { scan-assembler {call_then_avg:\n(?:[^\n]*\n)*?\tcall\ttick(?:@plt)?\n(?:[^\n]*\n)*?\tcsrwi\tvxrm,0\n} } } */

/* The two required writes account for every VXRM write in this file.  */
/* { dg-final { scan-assembler-times {\tcsrwi\tvxrm,} 2 } } */
