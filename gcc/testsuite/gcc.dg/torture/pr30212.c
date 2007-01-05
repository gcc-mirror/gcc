/* { dg-do compile } */

void test_crash (short *wb, int *dst)
{
  int is;
  int i;

  short *wBufSrc = wb;
  int *iBufDst = dst;

  for (i = 0; i < 2; i++)
  {  
    is =  (wBufSrc[ 0 > (i-1) ? 0 : (i -1 )]);

    iBufDst[i] =  is;
  }
}

int main(int argc, char** argv)
{
  short wb [] = { 1, 2, 3 };
  int   in [] = { 4, 5, 6 };

  test_crash(wb, in);

  return 0;
}
