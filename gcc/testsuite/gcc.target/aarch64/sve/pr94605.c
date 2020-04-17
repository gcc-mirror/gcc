/* { dg-options "-O2 -msve-vector-bits=256" } */

typedef int v8si __attribute__((vector_size(32)));
int g (v8si, v8si);

void
f (void)
{
  v8si x = {}, y = {};
  while (g (x, y))
    asm ("" : "+w" (x), "+w" (y));
}
