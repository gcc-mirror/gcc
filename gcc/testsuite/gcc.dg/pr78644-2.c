/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -finline-functions-called-once -w -Wno-psabi" } */

typedef unsigned V __attribute__ ((vector_size (64)));
typedef unsigned __int128 U __attribute__ ((vector_size (64)));

U
bar4 (U u0, U u1)
{
  if (u1[0])
    u1 <<= 1;
  return u0 + u1;
}

V
foo (U u, V v)
{
  v |= (unsigned)bar4(u, (U){})[0];
  return v;
}
