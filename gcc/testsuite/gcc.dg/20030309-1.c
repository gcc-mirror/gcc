/* { dg-do link } */
/* { dg-options "-O2" } */

struct A0 { int x; };
struct A1 { int x; int y[1]; };
struct A2 { int x; int y[2]; };
struct A3 { int x; int y[3]; };
struct A4 { int x; int y[4]; };

void *s;
int u;

int
main (void)
{
  int x;
  void *t = s;

  switch (u)
    {
    case 0:
      x = ((struct A0 *) t)->x;
      break;
    case 1:
      x = ((struct A1 *) t)->x;
      break;
    case 2:
      x = ((struct A2 *) t)->x;
      break;
    case 3:
      x = ((struct A3 *) t)->x;
      break;
    case 4:
      x = ((struct A4 *) t)->x;
      break;
    default:
      x = 0;
      break;
    }

  return x;
}
