/* PR target/8870 */
/* Originator: otaylor@redhat.com */
/* { dg-do compile } */
/* { dg-options "-O1 -mmmx -march=k8" } */

typedef short v4hi __attribute__ ((vector_size (8)));

static inline v4hi cvtsi_v4hi (int i)
{
  long long tmp = i;
  return (v4hi) tmp;
}

v4hi bar (unsigned short a)
{
  return cvtsi_v4hi (a);
}
