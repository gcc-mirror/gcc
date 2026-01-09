/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512fp16 " }*/
typedef __attribute__((__vector_size__ (8))) _Float16 F;
int a, b;
F f, *p;

void
foo ()
{
  do
    {
      f /= b ? *p : (F) {40};
l:
    }
  while (a);
  goto l;
}
