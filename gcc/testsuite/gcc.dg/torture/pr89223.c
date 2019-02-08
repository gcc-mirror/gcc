/* { dg-do compile { target int128 } } */

int a[5];
unsigned __int128 b;
void c()
{
  b = 4;
  for (;; b--)
    a[b] = ({ a[b + b]; });
}
