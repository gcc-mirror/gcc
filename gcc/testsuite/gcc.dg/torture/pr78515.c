/* { dg-do compile } */
/* { dg-additional-options "-Wno-psabi" } */
/* { dg-additional-options "-mavx512bw" { target x86_64-*-* i?86-*-* } } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

typedef unsigned V __attribute__ ((vector_size (64)));

V g;

static V
baz (V u, V v)
{
  g += u;
  return v + g + 1;
}

static V
bar (V u)
{
  u[0] = 0;
  return baz(u, (V){});
}

V
foo ()
{
  return (V){bar((V){})[0]};
}
