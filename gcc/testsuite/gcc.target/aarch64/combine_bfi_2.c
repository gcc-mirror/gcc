/* { dg-do compile } */
/* { dg-options "-O2" } */

int f1(int x, int y)
{
  return (y & 0xfffffff) | (((x <<28) & 0xf0000000));
}


int f2(int x, int y)
{
  return (((x <<28) & 0xf0000000)) | (y & 0xfffffff);
}

/* { dg-final { scan-assembler-times {\tbfi\t} 2 } } */
