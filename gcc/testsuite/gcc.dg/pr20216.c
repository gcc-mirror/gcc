/* { dg-do compile } */
/* { dg-options "-O1" } */

static unsigned int *buffer;

void FUNC (void)
{
 unsigned int *base;
 int i, j;

 for (i = 0; i < 4; i++)
  for (j = 0; j < 1600000; j++)
   *base++ = buffer[j];
}
