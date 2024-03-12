/* PR middle-end/103813 */
/* { dg-require-effective-target size32plus } */

struct A { char b; char c[0x21000000]; };
struct A d;

int
foo ()
{
  return d.c[0x20000000] || d.c[1];
}
