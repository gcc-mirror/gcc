/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-linear" } */

int a1[6][4][4];
short b1[16];

int c1;
void CalculateQuantParam(void)
{
  int i, j, k, temp;

   for(k=0; k<6; k++)
      for(j=0; j<4; j++)
        for(i=0; i<4; i++)
        {
          temp = (i<<2)+j;
          a1[k][j][i]  = c1/b1[temp];
        }
}

