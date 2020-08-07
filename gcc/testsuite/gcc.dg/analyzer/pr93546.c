/* { dg-do compile } */
/* { dg-require-effective-target label_values } */

void
ch (int x1)
{
  ({ bx: &&bx; });
  while (x1 == 0)
    {
    }
}
