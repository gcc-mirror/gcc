/* { dg-do compile } */

void a()
{
  for (int b; b; b = !b)
    ;
}
