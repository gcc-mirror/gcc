/* PR debug/45259 */
/* { dg-do compile } */
/* { dg-options "-g -O2 -w -fpic" { target fpic } } */
/* { dg-options "-g -O2 -w" { target { ! fpic } } } */

struct S { void (*bar) (long); };
struct T { struct S *t; };
int w;
extern int baz (int);

void
foo (int x, int u, char *z)
{
  struct T *v;
  static void *y[256] = { &&l1, &&l2 };
  for (;;)
    switch (x)
      {
      l2:
	x = 9;
      case 9:
	goto *y[*z++];
      case 10:
      case 27:
      case 54:
      case 99:
      case 100:
      case 120:
      case 122:
      case 131:
      case 132:
      case 134:
      case 141:
      case 142:
	v->t->bar (u);
	v->t->bar (u);
      case 143:
	continue;
      l1:
      default:
	baz (w);
      }
}
