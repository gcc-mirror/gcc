/* { dg-do run } */
/* { dg-options "" } */
/* { dg-additional-options "-std=c23" { target c } } */

unsigned char a[] = {
#embed __FILE__
};
struct S { unsigned char h[(sizeof (a) - 7) / 2]; short int i; unsigned char j[sizeof (a) - 7 - (sizeof (a) - 7) / 2]; };
struct T { int a, b, c; struct S d; long long e; double f; long long g; };
struct T b = {
#embed __FILE__
};

int
main ()
{
  if (b.a != a[0] || b.b != a[1] || b.c != a[2]
      || __builtin_memcmp (b.d.h, a + 3, sizeof (b.d.h))
      || b.d.i != a[3 + sizeof (b.d.h)]
      || __builtin_memcmp (b.d.j, a + 4 + sizeof (b.d.h), sizeof (b.d.j))
      || b.e != a[sizeof (a) - 3] || b.f != a[sizeof (a) - 2]
      || b.g != a[sizeof (a) - 1])
    __builtin_abort ();
}
