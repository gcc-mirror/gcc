/* { dg-do compile } */

typedef int v4si __attribute__ ((vector_size (16)));

int
foo (v4si v)
{
  return v[~0UL];
}
