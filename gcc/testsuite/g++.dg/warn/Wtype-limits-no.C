/* Test disabling -Wtype-limits.  */
/* { dg-do compile } */
/* { dg-options "-Wextra -Wno-type-limits" } */
extern void assert (int);

void a (unsigned char x)
{
  if (x < 0)  return;/* { dg-bogus "comparison is always false due to limited range of data type" } */
  if (x >= 0) return;/* { dg-bogus "comparison is always true due to limited range of data type" } */
  if (0 > x)  return;/* { dg-bogus "comparison is always false due to limited range of data type" } */
  if (0 <= x) return;/* { dg-bogus "comparison is always true due to limited range of data type" } */
  if (x <= 255) /* { dg-bogus "comparison is always true due to limited range of data type" } */
    return;
  if (255 >= x) /* { dg-bogus "comparison is always true due to limited range of data type" } */
    return;
  if ((int)x <= 255) 
    return;
  if (255 >= (unsigned char) 1)
    return;

}

void b (unsigned short x)
{
  if (x < 0)  return;/* { dg-bogus "comparison is always false due to limited range of data type" } */
  if (x >= 0) return;/* { dg-bogus "comparison is always true due to limited range of data type" } */
  if (0 > x)  return;/* { dg-bogus "comparison is always false due to limited range of data type" } */
  if (0 <= x) return;/* { dg-bogus "comparison is always true due to limited range of data type" } */
}

void c (unsigned int x)
{
  if (x < 0)  return;/* { dg-bogus "comparison of unsigned expression < 0 is always false" } */
  if (x >= 0) return;/* { dg-bogus "comparison of unsigned expression >= 0 is always true" } */
  if (0 > x)  return;/* { dg-bogus "comparison of unsigned expression < 0 is always false" } */
  if (0 <= x) return;/* { dg-bogus "comparison of unsigned expression >= 0 is always true" } */
  if (1U >= 0) return;
  if (1U < 0) return;
  if (0 <= 1U) return;
  if (0 > 1U) return;
}

void d (unsigned long x)
{
  if (x < 0)  return;/* { dg-bogus "comparison of unsigned expression < 0 is always false" } */
  if (x >= 0) return;/* { dg-bogus "comparison of unsigned expression >= 0 is always true" } */
  if (0 > x)  return;/* { dg-bogus "comparison of unsigned expression < 0 is always false" } */
  if (0 <= x) return;/* { dg-bogus "comparison of unsigned expression >= 0 is always true" } */
}

void e (unsigned long long x)
{
  if (x < 0)  return;/* { dg-bogus "comparison of unsigned expression < 0 is always false" } */
  if (x >= 0) return;/* { dg-bogus "comparison of unsigned expression >= 0 is always true" } */
  if (0 > x)  return;/* { dg-bogus "comparison of unsigned expression < 0 is always false" } */
  if (0 <= x) return;/* { dg-bogus "comparison of unsigned expression >= 0 is always true" } */
}

int test (int x) 
{
  if ((long long)x <= 0x123456789ABCLL) /* { dg-bogus "comparison is always true due to limited range of data type" } */
    return 1;
  else 
    return 0;
}

template <typename Int, Int D>
void f(Int x) {
  assert(0 <= x and x <= D); // { dg-bogus "comparison is always true due to limited range of data type" }
}

int ff(void) {
  f<unsigned char, 2>(5);
  f<signed char, 2>(5);
}

template <typename Int, Int D>
void g(void) {
  assert(0 <= D);
}
int gg(void) {
  g<unsigned char, 2>();
}

