/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

typedef struct tagPOINT
{
  int x;
  int y;
} POINT;

void
f (POINT * ptBuf)
{
  int i;
  for (i = 0; i < 4; i++)
    {
      ptBuf[i].x = ((ptBuf[i].x) << 4);
      ptBuf[i].y = ((ptBuf[i].y) << 4);
    }
}

