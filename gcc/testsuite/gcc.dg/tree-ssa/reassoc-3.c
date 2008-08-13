/* { dg-options "" } */
int main(int a, int b, int c, int d)
{
  int e = (a & ~b) & (~c & d);
  int f = (~c & a) & (b & ~d);
 return (e & f);
}
