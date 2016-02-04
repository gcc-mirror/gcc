/* { dg-do compile } */
/* { dg-additional-options "-w" } */

static int g_534[1][1];
int fn1()
{
  int i;
  for (i = 0; i < 4; i++)
    g_534[i + 2][i] ^= 3;
  for (;;)
    ;
}
