/* { dg-do compile } */

_Bool data[128];
void foo (_Bool *init)
{
  int i;
  for (i = 0; i < 128; i++)
    data[i] = *init;
}
