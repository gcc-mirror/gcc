/* PR target/39678 */
/* { dg-do run } */
/* { dg-options "-Wno-psabi" } */
struct Y {};
struct X {
  struct Y y;
  __complex__ float val;
};

struct X __attribute__((noinline))
foo (float *p)
{
  struct X x;
  __real x.val = p[0];
  __imag x.val = p[1];
  return x;
}
extern "C" void abort (void);
float a[2] = { 3., -2. };
int main()
{
  struct X x = foo(a);
  if (__real x.val != 3. || __imag x.val != -2.)
    abort ();
  return 0;
}
