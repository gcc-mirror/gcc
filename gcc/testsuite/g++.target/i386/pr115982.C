/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */

typedef float VF __attribute__((__vector_size__(16)));
typedef int VI __attribute__((__vector_size__(16)));

VI
foo (VF x)
{
  return !x;
}
