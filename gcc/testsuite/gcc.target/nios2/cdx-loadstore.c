/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mcdx" } */

/* Check generation of R2 CDX load/store instructions.  */

unsigned char ldb (unsigned char *p)
{
  return p[7];
}

unsigned short ldh (unsigned short *p)
{
  return p[7];
}

unsigned int ldw (unsigned int *p)
{
  return p[7];
}

void stb (unsigned char *p, unsigned char x)
{
  p[15] = x;
}

void sth (unsigned short *p, unsigned short x)
{
  p[15] = x;
}

void stw (unsigned int *p, unsigned int x)
{
  p[15] = x;
}

void no_cdx_stb (unsigned char *p, unsigned char x)
{
  p[16] = x;
}

void no_cdx_sth (unsigned short *p, unsigned short x)
{
  p[16] = x;
}

void no_cdx_stw (unsigned int *p, unsigned int x)
{
  p[16] = x;
}

/* { dg-final { scan-assembler "\tldbu\\.n\t.*, 7\\(.*\\)" } } */
/* { dg-final { scan-assembler "\tldhu\\.n\t.*, 14\\(.*\\)" } } */
/* { dg-final { scan-assembler "\tldw\\.n\t.*, 28\\(.*\\)" } } */

/* { dg-final { scan-assembler "\tstb\\.n\t.*, 15\\(.*\\)" } } */
/* { dg-final { scan-assembler "\tsth\\.n\t.*, 30\\(.*\\)" } } */
/* { dg-final { scan-assembler "\tstw\\.n\t.*, 60\\(.*\\)" } } */

/* { dg-final { scan-assembler "\tstb\t.*, 16\\(.*\\)" } } */
/* { dg-final { scan-assembler "\tsth\t.*, 32\\(.*\\)" } } */
/* { dg-final { scan-assembler "\tstw\t.*, 64\\(.*\\)" } } */
