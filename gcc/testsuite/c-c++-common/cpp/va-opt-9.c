/* PR preprocessor/89971 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++20" { target c++ } } */

int a, c;
#define m1(...) a /##__VA_OPT__(b) c
#define m2(...) a /##__VA_OPT__() c
#define m3(...) a##__VA_OPT__()##b = 1
#define m4(...) a##__VA_OPT__(b c d)##e = 2

int
foo (void)
{
  int d = m1();
  int e = m2(1);
  int m3(1 2 3);
  int m4();
  return d + e + ab + ae;
}
