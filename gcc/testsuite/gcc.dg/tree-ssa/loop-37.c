/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern void link_error (void);
static const int my_array [3] = { 4, 5, 6 };

void f0 (void)
{
  int j, sum = 0;
  for (j = 0; j < 3; j ++)
    sum += my_array [j];
  if (15 != sum)
    link_error ();
}

int f1 (int a [])
{
  int j, sum = 0;
  for (j = 0; j < 3; j ++)
    sum += a [j] + my_array [j];
  return sum;
}

int main() { }

/* { dg-final { scan-tree-dump-not "my_array" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
