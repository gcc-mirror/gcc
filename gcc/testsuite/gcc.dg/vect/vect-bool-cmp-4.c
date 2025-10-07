/* PR110223 */
/* { dg-do compile } */

_Bool k[1024];
_Bool res[1024];

int main ()
{
  char i;
  for (i = 0; i < 64; i++)
    res[i] = k[i] != (i == 0);
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" } } */
