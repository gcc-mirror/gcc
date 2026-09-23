/* { dg-do run } */

#define NI __attribute((noipa))

typedef char U __attribute__ ((vector_size (16)));

NI void fun (long i, U u)
{
  (void) i;
  if (u[0] != 1)
    __builtin_abort ();
}

U u = { 1 };

int main (void)
{
  fun (0, u);

  return 0;
}
