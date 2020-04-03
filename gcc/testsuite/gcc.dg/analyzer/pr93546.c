/* { dg-do compile } */

void
ch (int x1)
{
  ({ bx: &&bx; });
  while (x1 == 0)
    {
    }
}
