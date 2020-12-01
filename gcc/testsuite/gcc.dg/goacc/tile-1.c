/* { dg-do compile } */

/* PR c/97880 */

void f ()
{
  #pragma acc parallel loop tile(2, 3)
  for (int i = 0; i < 8; i++)
    for (long j = 0; j < 8; j++);
}
