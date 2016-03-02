/* { dg-do compile } */
/* { dg-options "-w -Wno-psabi" } */

typedef int v4si __attribute__ ((vector_size (16)));

int
foo (v4si v)
{
  return v[~0UL];
}
