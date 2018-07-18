/* PR sanitizer/80168 */
/* { dg-do compile } */

int a;

int
foo (void)
{
  struct S { int c[a]; int q : 8; int e : 4; } f;
  f.e = 4;
  return f.e;
}
