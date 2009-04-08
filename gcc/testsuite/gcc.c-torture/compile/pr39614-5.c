/* { dg-do compile } */
/* { dg-options "-w -std=c99" } */
int i;
void
f (void)
{
  i = (1 / 0, 1 / 0);
}
