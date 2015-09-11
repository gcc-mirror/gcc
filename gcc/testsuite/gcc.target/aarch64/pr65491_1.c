/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef long double a __attribute__((vector_size (16)));

a
sum (a first, a second)
{
  return first + second;
}

