/* { dg-do compile } */
/* { dg-options "-O2 -w -Wno-psabi" } */

typedef unsigned int v4si __attribute__((vector_size(16)));
typedef unsigned int v2si __attribute__((vector_size(8)));

/* The aliasing is somewhat dubious here, but it must compile.  */

v2si
foo (v4si v)
{
  v2si res;
  *(v4si *) &res = v;
  return res;
}
