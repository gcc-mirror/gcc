/* { dg-do run } */

__attribute__((__vector_size__ (sizeof (long unsigned) * 8))) long unsigned b;

void __attribute__((noipa))
foo ()
{
  b += __builtin_assoc_barrier (b);
}

int main()
{
  int i;
  for (i = 0; i < 8; ++i)
    b[i] = i;
  foo ();
  for (i = 0; i < 8; ++i)
    if (b[i] != 2*i)
      __builtin_abort ();
  return 0;
}
