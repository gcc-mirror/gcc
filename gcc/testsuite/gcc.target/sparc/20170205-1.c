/* PR target/79363 */
/* Reported by John Paul Adrian Glaubitz <glaubitz@physik.fu-berlin.de> */

/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mcpu=v8" } */

struct d { long long h; };

struct c { struct d *e; };

int f, g;

extern void bar (long long *);
extern int baz (long long *, int);

void foo (struct c *desc)
{
  int begin, end, j;
  long long k, l, h;
  for (;;) {
    for (;;)
      break;
    for (;;) {
      j++;
      l = desc->e[j].h;
      for (;;) {
        bar(&l);
        end = h = begin / 2;
        if (baz(&h, g))
          begin = f;
        break;
      }
      if (end) {
        __atomic_store_n(&k, end, 5);
        break;
      }
    }
  }
}
