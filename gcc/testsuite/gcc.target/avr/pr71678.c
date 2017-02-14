/* { dg-do compile } */
/* { dg-options "-Os -fno-tree-switch-conversion" } */

unsigned char foo (long long x) 
{
  unsigned char y = 0;
  switch (x)
    {
    case 0: y = 67; break;
    case 1: y = 20; break;
    case 2: y = 109; break;
    case 3: y = 33; break;
    case 4: y = 44; break;
    case 5: y = 37; break;
    case 6: y = 10; break;
    case 7: y = 11; break;
    }
  return y;
}
