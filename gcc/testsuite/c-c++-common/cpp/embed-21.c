/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi" } */
/* { dg-additional-options "-std=c23" { target c } } */

unsigned char a[] = {
#embed __FILE__
};
const unsigned char b[] = {
#embed __FILE__
};
unsigned char c[] = {
  0, 1, 2, 3, 4, 5, 6, 7,
#embed __FILE__ limit(128) suffix (,)
#embed __FILE__ limit(128) suffix (,)
#embed __FILE__
};
const unsigned char d[] = {
  0, 1, 2, 3, 4, 5, 6, 7,
#embed __FILE__ limit(128) suffix (,)
#embed __FILE__ limit(128) suffix (,)
#embed __FILE__
};
typedef char V __attribute__((vector_size (16), may_alias));
struct __attribute__((may_alias)) S { int a, b, c, d; };

__attribute__((noipa)) int
foo (V x, V y)
{
  return __builtin_memcmp (&x, &y, sizeof (x));
}

__attribute__((noipa)) int
bar (struct S x, struct S y)
{
  return x.a != y.a || x.b != y.b || x.c != y.c || x.d != y.d;
}

int
main ()
{
  if (a[0] != b[0]
      || a[42] != b[42]
      || a[sizeof (a) - 5] != b[sizeof (a) - 5]
      || a[sizeof (a) - 1] != b[sizeof (a) - 1])
    __builtin_abort ();
  if (foo (((V *) a)[0], ((V *) b)[0])
      || foo (((V *) a)[1], ((V *) b)[1])
      || foo (((V *) c)[8], ((V *) d)[8])
      || foo (((V *) c)[9], ((V *) d)[9]))
    __builtin_abort ();
  if (bar (((struct S *) a)[0], ((struct S *) b)[0])
      || bar (((struct S *) a)[1], ((struct S *) b)[1])
      || bar (((struct S *) c)[8], ((struct S *) d)[8])
      || bar (((struct S *) c)[9], ((struct S *) d)[9]))
    __builtin_abort ();
}
