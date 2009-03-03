/* PR tree-optimization/39343 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

extern inline __attribute__ ((__always_inline__)) int
foo (char *dest)
{
  return __builtin_object_size (dest, 1);
}

struct S
{
  union
  {
    struct { int a, b; char c, d; } f;
    struct { struct { int a, b; char c, d[255]; } e; } g;
  } u;
};

int
main (void)
{
  struct S s;
  if (foo (s.u.g.e.d) != 255)
    abort ();
  return 0;
}
