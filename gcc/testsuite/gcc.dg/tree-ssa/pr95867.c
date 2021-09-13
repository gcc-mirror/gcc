/* PR tree-optimization/95867 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " \\* " 13 "optimized" } } */

#define A n * n * n * n * n * n * n * n
#define B A * A * A * A * A * A * A * A
#define C B * B * B * B * B * B * B * B

unsigned
foo (unsigned n)
{
  return C * B * B * A * n * n * n * n * n;
}
