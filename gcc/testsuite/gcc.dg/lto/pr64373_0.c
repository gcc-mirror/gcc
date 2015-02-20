/* { dg-lto-do assemble } */

extern void b(int L, float (*data)[L]);

void a(void)
{
  float* p = 0;
  int i = 0;
  b(10, (float (*)[10])(p + i));
}
