/* PR target/39678 */
/* { dg-do run } */
/* { dg-options "-Wno-psabi" } */

struct X {
  signed char c;
  __complex__ float val;
};

struct X __attribute__((noinline))
foo (float *p)
{
  struct X x;
  x.c = -3;
  __real x.val = p[0];
  __imag x.val = p[1];
  return x;
}
extern void abort (void);
float a[2] = { 3., -2. };
int main()
{
  struct X x = foo(a);
  if (x.c != -3 || __real x.val != a[0] || __imag x.val != a[1])
    abort ();
  return 0;
}
