/* PR target/100028 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#define W	3
#define L	11

int
foo (int d, int s)
{
  int wmask = (1 << W) - 1;
  return (d & ~wmask) | ((s >> L) & wmask);
}

long long int
bar (long long int d, long long int s)
{
  long long int wmask = (1LL << W) - 1;
  return (d & ~wmask) | ((s >> L) & wmask);
}

/* { dg-final { scan-assembler-times {\tbfxil\t} 2 } } */
