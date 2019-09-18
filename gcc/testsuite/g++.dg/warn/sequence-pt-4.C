/* More sequence point warning tests  */
/* { dg-do compile } */
/* { dg-options "-Wsequence-point" } */

struct S { int a[10]; };
void bar (int, int, int, int, int, int, int, int);

int
foo (int i, int x[10][10], int y[10], struct S z[10], struct S *w[10])
{
  int b = x[i++][i++];	/* { dg-warning "undefined" "sequence point warning" { target c++14_down } } */
  int c = i++ << i++;	/* { dg-warning "undefined" "sequence point warning" { target c++14_down } } */
  int d = i++ >> i++;	/* { dg-warning "undefined" "sequence point warning" { target c++14_down } } */
  int e = i++ && i++;
  int f = i++ ? i++ : i++;
  int g = (i++, i++);
  int h = z[i++].a[i++];	/* { dg-warning "undefined" "sequence point warning" { target c++14_down } } */
  int j = w[i++]->a[i++];	/* { dg-warning "undefined" "sequence point warning" { target c++14_down } } */
  bar (b, c, d, e, f,g, h, j);
  return i;
}
