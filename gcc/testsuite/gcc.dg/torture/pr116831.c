/* { dg-additional-options "-mcpu=neoverse-v2" { target aarch64*-*-* } } */

long a;
int b, c;
void d (int e[][5], short f[][5][5][5]) 
{
  for (short g; g; g += 4)
    a = c ?: e[6][0] % b ? 0 : f[0][0][0][g];
}

