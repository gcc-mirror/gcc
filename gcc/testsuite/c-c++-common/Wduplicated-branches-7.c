/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

struct S
{
  int x;
} s;
int a[10];

#define XMEM(R) ((R).x)
#define XSTR(R) ((R).x)

void
f (int i)
{
  if (i)
    XMEM(s) = 1;
  else
    XSTR(s) = 1;

  if (i) /* { dg-warning "this condition has identical branches" } */
    s.x = 1;
  else
    s.x = 1;

  if (i)
    XMEM(s) = 1;
  else
    s.x = 1;

  if (i)
    s.x = 1;
  else
    XMEM(s) = 1;
}
