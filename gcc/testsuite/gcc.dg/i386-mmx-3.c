/* PR target/8870 */
/* Originator: otaylor@redhat.com */
/* { dg-do compile { target i?86-*-* x86_64-*-*} } */
/* { dg-options "-O1 -mmmx -march=athlon" } */

typedef int v4hi __attribute__ ((mode (V4HI)));

static inline v4hi cvtsi_v4hi (int i)
{
  long long tmp = i;
  return (v4hi) tmp;
}

v4hi bar (unsigned short a)
{
  return cvtsi_v4hi (a);
}
