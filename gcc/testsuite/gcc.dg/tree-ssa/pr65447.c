/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

void foo (double *p)
{
  int i;
  for (i = -20000; i < 200000; i+= 40)
    {
      p[i+0] = 1.0;
      p[i+1] = 1.0;
      p[i+2] = 1.0;
      p[i+3] = 1.0;
      p[i+4] = 1.0;
      p[i+5] = 1.0;
      p[i+6] = 1.0;
      p[i+7] = 1.0;
      p[i+8] = 1.0;
      p[i+9] = 1.0;
      p[i+10] = 1.0;
      p[i+11] = 1.0;
      p[i+12] = 1.0;
      p[i+13] = 1.0;
      p[i+14] = 1.0;
      p[i+15] = 1.0;
      p[i+16] = 1.0;
      p[i+17] = 1.0;
      p[i+18] = 1.0;
      p[i+19] = 1.0;
      p[i+20] = 1.0;
      p[i+21] = 1.0;
      p[i+22] = 1.0;
      p[i+23] = 1.0;
      p[i+24] = 1.0;
      p[i+25] = 1.0;
      p[i+26] = 1.0;
      p[i+27] = 1.0;
      p[i+28] = 1.0;
      p[i+29] = 1.0;
      p[i+30] = 1.0;
      p[i+31] = 1.0;
      p[i+32] = 1.0;
      p[i+33] = 1.0;
      p[i+34] = 1.0;
      p[i+35] = 1.0;
      p[i+36] = 1.0;
      p[i+37] = 1.0;
      p[i+38] = 1.0;
      p[i+39] = 1.0;
    }
}

/* We should groups address type IV uses.  */
/* { dg-final { scan-tree-dump-not "\\nuse 21\\n" "ivopts" } }  */
