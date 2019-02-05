/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-options "-O2 -Wno-psabi" } */
/* { dg-additional-options "-msse" { target i?86-*-* x86_64-*-* } } */

typedef int v4si
  __attribute__ ((vector_size (16)));

int fn1 (v4si p)
{
  return p[0];
}

