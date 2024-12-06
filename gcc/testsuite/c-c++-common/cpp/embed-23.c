/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-std=gnu23" { target c } } */

typedef unsigned char V __attribute__((vector_size (16)));

struct S { _Complex double a; V b; int c; };
struct T { int a; struct S b; int c; struct S d; int e; unsigned char f[22]; _Complex long double g; };

const unsigned char a[] = {
  #embed __FILE__ limit (124)
};
const struct T b[2] = {
  #embed __FILE__ limit (124)
};

int
main ()
{
  for (int i = 0; i < 2; ++i)
    if (b[i].a != a[i * 62]
	|| __real__ b[i].b.a != a[i * 62 + 1]
	|| __imag__ b[i].b.a
	|| __builtin_memcmp (&b[i].b.b, &a[i * 62 + 2], 16)
	|| b[i].b.c != a[i * 62 + 18]
	|| b[i].c != a[i * 62 + 19]
	|| __real__ b[i].d.a != a[i * 62 + 20]
	|| __imag__ b[i].d.a
	|| __builtin_memcmp (&b[i].d.b, &a[i * 62 + 21], 16)
	|| b[i].d.c != a[i * 62 + 37]
	|| b[i].e != a[i * 62 + 38]
	|| __builtin_memcmp (&b[i].f[0], &a[i * 62 + 39], 22)
	|| __real__ b[i].g != a[i * 62 + 61]
	|| __imag__ b[i].g)
      __builtin_abort ();
}
