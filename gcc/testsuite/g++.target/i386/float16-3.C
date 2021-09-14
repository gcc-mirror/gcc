/* { dg-do assemble { target avx512fp16 } } */
/* { dg-options "-O0 -mavx512fp16" } */

template <typename> void a(char *) {}
char b, d;
void c()
{
  a<unsigned char>(&d);
  a<_Float16>(&b);
}
