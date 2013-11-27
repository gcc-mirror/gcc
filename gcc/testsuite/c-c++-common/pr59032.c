/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo()
{
  float v __attribute__((vector_size(8)));
  v++;
}

void
foo2 ()
{
  float v __attribute__((vector_size(8)));
  ++v;
}

void
foo3 ()
{
  float v __attribute__((vector_size(8)));
  v--;
}

void
foo4 ()
{
  float v __attribute__((vector_size(8)));
  --v;
}
