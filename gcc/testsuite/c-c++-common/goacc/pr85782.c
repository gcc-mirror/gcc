/* PR c++/85782 */

void
foo ()
{
  int i;
  
  #pragma acc parallel loop
  for (i = 0; i < 100; i++)
    continue;
}
