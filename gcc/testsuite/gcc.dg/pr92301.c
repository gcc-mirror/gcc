/* { dg-do run } */
/* { dg-options "-O3" } */

unsigned int m;

#define N 128
unsigned int a[N];

unsigned int
__attribute__((noipa))
df_count_refs (_Bool include_defs)
{
  int size = 0;

  for (unsigned int regno = 0; regno < m; regno++)
    if (include_defs)
      size += a[regno];
  return size;
}

int main(int argc, char **argv)
{
  for (unsigned i = 0; i < N; i++)
    a[i] = i;

  if (argc < 2)
    m = 17;

  unsigned int r = df_count_refs(1);
  __builtin_printf ("r: %d\n", r);
  if (r != 136)
    __builtin_abort ();

  return 0;
}
